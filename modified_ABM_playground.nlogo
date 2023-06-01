extensions [table csv]

breed [children child]

children-own
[
  girl?
  beauty
  kindness
  shyness
  power
  status
  hbn-table                     ;; table, each child has a value for each other child in simulation
  favorite                      ;; could potentially be turned into a local variable, but it's complicated
  mate                          ;; not really state; used temporarily during interaction
  can-play?                     ;; set to true at the start of procedure go; set to false at start of play
  I-won?                        ;; to show outcome of fight. returned to play-with-mate by perform-fight
]

globals
[
  ;;; CSV
  csv-list

  ;;;; dynamic variables
  girls
  boys
  girl-groups
  boy-groups
  mixed-groups
  avg-boy-status
  avg-girl-status
  gender-status-gap

  ;;;; counters -- all implicitly reset to 0 during setup
  n-groups-ever
  n-conferrals-ever
  n-fights-ever
  n-unhappy-but-not-fighting-ever
  n-happy-ever
  n-fights-blamed
  n-fights-approved
  n-group-disband
  n-abandoner
  n-rejection

  ;;;; parameters (in interface)
  ; --- sliders ---
  ; n-boys
  ; n-girls
  ; girl-beauty-ratio
  ; girl-kindness-ratio
  ; girl-shyness-ratio
  ; girl-power-ratio
  ; beauty-stdev
  ; kindness-stdev
  ; shyness-stdev
  ; power-stdev
  ; individualism
  ; power-distance
  ; uncertainty-avoidance
  ; calling-distance
  ; intergroup-gap
  ; interchild-gap
  ; conferral-stdev
  ; R&T-stdev                   ;; R&T stands for rough-and-tumble everywhere in this code
  ; personality-ratio
  ; status-ratio
  ; relationship-ratio
  ; girl-R&T-ratio
  ; volatility                  ;; of status and hbn updates. assumed to be the same, for cultural reasons (LTO)
  ; rejection-penalty
  ; fight-penalty
  ; fight-tolerance
  ; fight-threshold
  ; R&T-augmenter
  ; --- input boxes ---
  ; recess-length-in-ticks
  ; current-seed
  ; --- switches ---
  ; new-seed?
  ; sex-factor-on-conferral?
  ; R&T-offence?
  ; R&T-by-status?
  ; record-movie?               ;; recording movie, does not influence simulation
  ; debug?                      ;; output debug information, does not influence simulation (take care of that!)

  ;;;; parameters (in code)
  background-color
  alone-color
  lowerbound-average
  upperbound-average
  min-distance
  R&T-threshold                 ;; which difference in power is seen as "slightly lower"?
  basic-R&T-acceptance
  hbn-weight-in-R&T-acceptance
  min-beauty
  max-beauty
  min-kindness
  max-kindness
  min-shyness
  max-shyness
  min-power
  max-power
  min-status
  max-status
  min-hbn
  max-hbn

  ;;;; meta parameter for documenting parameters file
  experiment-id
  ;;;; meta parameters for sensitivity analysis
  parameter-name
  parameter-value
  parameter-default-value

  ;;;; static variables, derived from parameters
  recess-length
  personality-weight            ;; internal weighing of conferral
  status-weight                 ;; internal weighing of conferral
  hbn-weight                    ;; internal weighing of conferral (has-been-nice)
  shyness-weight     ;; trying to add a weight here for later use in fighting tendency
]

;;
;; SETUP
;;

to set-parameters ; observer procedure
  set background-color sky + 2.5
  set alone-color grey
  set lowerbound-average 0.4
  set upperbound-average 0.6
  set min-distance 2
  set R&T-threshold 0.2
  set basic-R&T-acceptance 0.25
  set hbn-weight-in-R&T-acceptance 0.5
  set min-beauty 0
  set max-beauty 1
  set min-kindness 0
  set max-kindness 1
  set min-shyness 0
  set max-shyness 1
  set min-power 0
  set max-power 1
  set min-status 0
  set max-status 1
  set min-hbn -1
  set max-hbn 1

  set experiment-id "?"
  set parameter-name ""
  set parameter-value 0
  set parameter-default-value 0
  read-parameters ; see end

  set recess-length recess-length-in-ticks
  ;; convert slider values from interface into scaled weights
  let total (relationship-ratio + personality-ratio + status-ratio + shyness-ratio)
  set personality-weight (personality-ratio / total)
  set status-weight (status-ratio / total)
  set hbn-weight (relationship-ratio / total)
  set shyness-weight (shyness-ratio / total)
end

to setup ; observer procedure
  clear-all
  if new-seed? [ set current-seed new-seed ]
  random-seed current-seed
  set-parameters
  set gender-status-gap 0
  setup-patches
  setup-children
  set csv-list []
  reset-ticks
end

to setup-patches ; observer procedure
  ask patches [ set pcolor background-color ]
end

to setup-children ; observer procedure
  create-children n-boys
  [
    set girl? false
    set power random-normal-in-domain (1 - girl-power-ratio) power-stdev min-power max-power
    set beauty random-normal-in-domain (1 - girl-beauty-ratio) beauty-stdev min-beauty max-beauty
    set kindness random-normal-in-domain (1 - girl-kindness-ratio) kindness-stdev min-kindness max-kindness
    set shyness random-normal-in-domain (1 - girl-shyness-ratio) shyness-stdev min-shyness max-shyness
    set-shape
  ]
  create-children n-girls
  [
    set girl? true
    set power random-normal girl-power-ratio power-stdev
    set beauty random-normal girl-beauty-ratio beauty-stdev
    set kindness random-normal girl-kindness-ratio kindness-stdev
    set shyness random-normal girl-shyness-ratio shyness-stdev
    set-shape
  ]
  set girls children with [girl?]
  set boys children with [not girl?]
  ask children
  [
    if beauty > max-beauty [ set beauty max-beauty ]
    if beauty < min-beauty [ set beauty min-beauty ]
    if kindness > max-kindness [ set kindness max-kindness ]
    if kindness < min-kindness [ set kindness min-kindness ]
    if power > max-power [ set power max-power ]
    if power < min-power [ set power min-power ]
    if shyness > max-shyness [ set shyness max-shyness ]
    if shyness < min-shyness [ set shyness min-shyness ]
    set label (word round (10 * power) " " who)                 ;; for the interface, show power and who number
    set-alone
    set status 0.5                    ;; start with neutral status; they do not know one another yet
    redraw-myself                                               ;; to show status through size
    setxy random-pxcor random-pycor
    setup-hbn-table self
  ]
end

to set-shape ; child procedure
  let part1 "average-"
  if (beauty - min-beauty) < lowerbound-average * (max-beauty - min-beauty) [ set part1 "ugly-" ]
  if (beauty - min-beauty) > upperbound-average * (max-beauty - min-beauty) [ set part1 "beauty-" ]
  let part2 "average-"
  if (kindness - min-kindness) < lowerbound-average * (max-kindness - min-kindness)  [ set part2 "unkind-" ]
  if (kindness - min-kindness) > upperbound-average * (max-kindness - min-kindness) [ set part2 "kind-" ]
  let part3 "neutral" ; should not persist
  ifelse girl?
  [ set part3 "girl" ]
  [ set part3 "boy" ]
  set shape (word part1 part2 part3)
end

;;
;; MAIN PROCEDURES
;;

;; perform one complete recess
to run-recess ; observer procedure
  setup
  repeat recess-length [ go ]
end

to go ; observer procedure
  if debug? [ show "start of tick" ]
  ask children [ set can-play? true ]
  ask children
  [
    ; if debug? [ show "it's my turn" ]
    if can-play?
    [
      ; if debug? [ show "I can play" ]
      if alone? [ find-friend-or-group ]
      choose-mate
      if mate != nobody                         ;; all the group's children might already be playing
      [
        ask mate [ set mate myself]             ;; so we are one another's mate
        play-with-mate
      ]
    ]
    ; if debug? [ show "it's the end of my turn" ]
  ]
  ask children                                  ;; groups and children re-position themselves for visibility
  [
    redraw-myself
    reposition-myself
  ]
  update-outputs
  if record-movie? [ record-frame ]
  tick
  prep-csv
  write-csv
end

to find-friend-or-group ; child procedure gj 2016 03 31
  ifelse anyone-in-calling-distance?
  [
    find-favorite
    ifelse favorite != nobody                                                   ;; I found an attractive child
    [
      ifelse attractive-to? favorite                                            ;; that child attracted to me in return?
      [
        ifelse [alone?] of favorite                                             ;; yes, favorite likes me
        [ create-group-with-favorite ]
        [ join-group-of-favorite ]
      ]
      [                                                                         ;; favorite does not like me and I resent that
        decrease-hbn-of-favorite
        step-back-from favorite
        set favorite nobody
        monitor-rejection
      ]
    ]
    [ flee ]                                                                    ;; could not find a suitable play mate
  ]
  [ flee ]                                                                      ;; there's nobody near me
end

to play-with-mate ; child procedure
  if (mate = nobody) or not (can-play? and [can-play?] of mate)
  [ error "trying to play while unable to play" ]
  set can-play? false
  ask mate [ set can-play? false ]
  let initial-mate-status [status] of mate      ;; for use in update-hbn further down

  ;; actual play process starts here
  let conferral determine-conferral
  ifelse mate-happy? conferral
  [
    mate-accepts-conferral conferral
    monitor-happy
  ]
  [                                             ;; mate unhappy with conferral
    ifelse mate-will-fight?
    [
      ;; mate picks fight
      ifelse fighting-condemned? conferral      ;; fight outcome irrelevant when fighting condemned
      [
        mate-gets-fight-penalty
        ask mate [ consider-leaving-group ]
        monitor-condemned-fights
      ]
      [                                         ;; fighting condoned by group, so it matters who wins
        set I-won? true
        perform-fight conferral                 ;; determines outcome and new statuses
        if debug? [ show "I-won: " show I-won? ]
        ifelse I-won?
        [
          ask mate [ consider-leaving-group ]
        ]
        [
          consider-leaving-group
        ]
        monitor-condoned-fights
      ]
    ]
    [                                           ;; mate grumblingly accepts
      mate-accepts-conferral conferral
      ask mate
      [
        consider-leaving-group
        step-back-from myself
      ]
      monitor-unhappy-but-not-fighting
    ]
  ]
  update-hbn mate self (conferral - initial-mate-status)
  monitor-conferrals
end

;;
;; SUBPROCEDURES
;;

to-report anyone-in-calling-distance?
  report any? visible-others
end

to find-favorite
  set favorite nobody
  let candidates visible-others with [attractive-to? myself]
  set favorite max-one-of candidates [attractiveness-factor myself]
end

to decrease-hbn-of-favorite ; child procedure
  decrease-hbn self favorite rejection-penalty
end

;; choose another agent of same color to execute play function.
to choose-mate ; child procedure
  ifelse alone?
  [
    set mate nobody
  ]
  [
    let my-color color
    set mate one-of other children with [can-play? and (color = my-color)]
  ]
end

to-report mate-happy? [conferral] ; child reporter
  report [status] of mate <= conferral
end

to mate-accepts-conferral [conferral] ; child procedure
  let inviting-agent-status status
  ask mate [
    set-status (status + (status-impact inviting-agent-status) * (conferral - status) * volatility)
  ]
end

to-report mate-will-fight? ; child reporter
  let scorn (fight-tolerance-0-1 * (power-difference mate self) + power-distance-0-1 * (status-difference mate self)) / 2
  let hbn-value hbn-factor self mate
  let shyness-influence shyness-weight * ([shyness] of mate)
  let fight-tendency (personality-weight * (1 - [kindness] of mate) + status-weight * scorn - hbn-weight * hbn-value - shyness-influence) ;;letting shyness impact willingness to fight
  report fight-tendency > fight-threshold
end

to-report fighting-condemned? [conferral] ; child reporter
  let perceived-affront [status] of mate - conferral
  let sex-gap? (ifelse-value sex-difference? mate self [1] [0])
  let gender-influence (sex-gap? * segregation-tendency + (1 - fight-tolerance-0-1)) / 2
  let probability (1 - perceived-affront - gender-influence + (status-difference mate self) * power-distance-0-1)
  report random-float 1 > probability
end

to mate-gets-fight-penalty ; child procedure
  ask mate [ set status status * (1 - fight-penalty) ]
end

to perform-fight [conferral] ; child procedure
  let giver self
  let affront [status] of mate - conferral
  let power-giver [power] of giver
  let power-receiver [power] of mate
  let receiver-wins? power-receiver > power-giver - random-normal affront conferral-stdev
  set I-won? not receiver-wins?
  let winner ifelse-value receiver-wins? [mate] [giver]
  let loser ifelse-value receiver-wins? [giver] [mate]
  ask winner [
    let target-status status-impact [status] of loser
    set-status (status + target-status * volatility)
  ]
  ask loser
  [
    let target-status status-impact (1 - [status] of winner)
    set-status (status - target-status * volatility)
  ]
end

to consider-leaving-group  ; child procedure
  let status-contribution (status-difference mate self) * power-distance-0-1    ;; scale -1:+1
  set status-contribution 1 + status-contribution                               ;; scale 0:2
  let status-importance (collectivism-0-1 + status-contribution) / 3
  let probability (status-weight * status-importance + personality-weight * kindness + hbn-weight * hbn-factor mate self + shyness-weight * shyness) ;;letting shyness impact whether the child will leave
  if random-float 1 > probability
  [ leave-group ]
end

;; if one other group member disband group; otherwise only the individual leaves
to leave-group ; child procedure
  let mycolor color
  let rest-of-group other children with [color = mycolor]
  if count rest-of-group = 1
  [
    ask rest-of-group [ set-alone ]
    set n-group-disband n-group-disband + 1
  ]
  set-alone
  set n-abandoner n-abandoner + 1
  avoid-all-groups
end

to-report determine-conferral ; child reporter
  let conferral determine-basic-conferral
  if R&T-augmenter > 0 ; is R&T on?
  [
    if do-R&T?
    [
      set conferral process-R&T conferral
    ]
  ]
  if conferral > max-status [ set conferral max-status ]
  if conferral < min-status [ set conferral min-status ]
  report conferral
end

to-report determine-basic-conferral; child reporter
  let n ( (0.33 * [status] of mate) ;;status-weight manually adjusted specifically here. Based on literature on exsisting likeability tied together with status
        + (0.33 * (kindness + [beauty] of mate) / 2) ;;personality-weight manually adjusted specifically here. Based on literature on likeability having a positive effect on status
        + (0.33 * hbn-factor-normalized mate self)) ;;hbn-weight
  if [shyness] of mate >= 0.5 [
    set n (n - (shyness-weight * shyness) / 2)
]  ;; Making shyness have an impact if shyness is above average, but not too strong an impact.
  if sex-factor-on-conferral?
  [
    if debug?
    [
      show "sex-factor on. status giver"
      ask mate [ show "and status receiver" ]
    ]
    if sex-difference? self mate
    [
      if debug? [ show "mixed-sex conferral; n = " show n show fight-tolerance ]
      ifelse girl?
      [ set n (n + gender-status-gap / 2) ]
      [ set n (n - gender-status-gap / 2) ]
      if debug? [ show " n after update = " show n ]
    ]
  ]
  let conferral random-normal n conferral-stdev
  report conferral
end

;;
;; ROUGH_AND_TUMBLE
;;

to-report do-R&T? ; child procedure
  ifelse R&T-by-status?                                         ;; R&T-by-status in line with literature
  [
    let my-status-fuzzy random-normal status R&T-stdev
    report ([status] of mate < my-status-fuzzy) and (my-status-fuzzy < [status] of mate + R&T-threshold)
  ]
  [
    let my-power-fuzzy random-normal power R&T-stdev
    report ([power] of mate < my-power-fuzzy) and (my-power-fuzzy < [power] of mate + R&T-threshold)
  ]
end

to-report process-R&T [conferral] ; child reporter
  let R&T-value determine-R&T-value conferral
  let R&T-interpretation interpret-R&T mate self
  ifelse random-float 1 > R&T-interpretation
  [
    set conferral conferral + R&T-value
  ]
  [
    if R&T-offence?
    [
      set conferral conferral - R&T-value
    ]
  ]
  report conferral
end

;; girls do less R&T, and like it less. boys do it to slightly less powerful others. Value is surplus conferral.
to-report determine-R&T-value [conferral] ; child reporter
  let n 0
  let R&T-factor ifelse-value girl? [girl-R&T-ratio] [1.0]
  let shyness-factor (shyness-weight * shyness)                             ;; Shyness factor for liking R&T less
  set n random-normal (R&T-augmenter * conferral * R&T-factor * shyness-factor) R&T-stdev
  if n > power [ set n power ]
  if n < min-power [ set n min-power ]
  report n
end

;; GJH 2016 03 08: changed name from interpret-R&T-value: the value is not interpreted, only the facto of doing the R&T
to-report interpret-R&T [receiver giver] ; generalized reporter
  ifelse [girl?] of receiver
  [ report basic-R&T-acceptance - hbn-weight-in-R&T-acceptance * (hbn-factor giver receiver) + (1 - girl-R&T-ratio) + (1 - shyness-R&T-ratio) ] ;;added shyness to influence interpreting doing R&T
  [ report basic-R&T-acceptance - hbn-weight-in-R&T-acceptance * (hbn-factor giver receiver) + (1 - shyness-R&T-ratio) ]
end

;;
;; HELPER PROCEDURES AND REPORTERS
;;

to-report attractive-to? [other-child] ; child reporter. Calling child is subject, other-child is object
  let attractiveness-threshold (personality-weight + hbn-weight) / 2
  report (attractiveness-factor other-child) > attractiveness-threshold
end

;; *-weights add up to 1 and are same weights used for conferral-value
to-report attractiveness-factor [other-child] ; child reporter
  let me self
  let you other-child
  let status-term      0.33      * ([status] of you - [status] of me + 1) / 2 ;; status-weight
  let personality-term 0.33 * ([kindness] of me + [beauty] of you) / 2 ;; personality-weight
  let hbn-term         0.33         * (hbn-factor me you + 1) / 2 ;; my perception of your niceness - hbn-weight
  report status-term + personality-term + hbn-term
end

to-report power-difference [child1 child2] ; generalized reporter
  report [power] of child1 - [power] of child2                  ;; range -1:1
end

to-report status-difference [child1 child2] ; generalized reporter
  report [status] of child1 - [status] of child2                ;; range -1:1
end

to-report hbn-factor [owner other-child] ; generalized reporter
  report get-hbn owner other-child                              ;; range -1:1
end

to-report hbn-factor-normalized [owner other-child] ; generalized reporter
  report (hbn-factor owner other-child + 1) / 2                 ;; normalized range 0:1
end

to-report status-impact [other-status] ; generalized reporter 0-1 moderating impact of status depending on PDI
  let egalitarianism 1 - power-distance-0-1
  report egalitarianism + (1 - egalitarianism) * other-status
end

;; segregation-tendency is the tendency to infer group boundaries from differences in past perceived status conferrals from members of a reference group.
;; A function of culture: average of Collectivism and Uncertainty avoidance scaled to 0-1
;; Actually these functions are the link to literature and to face validity, and for cross-cultural work they are going to need tweaking.
to-report segregation-tendency ; child procedure
  report (collectivism-0-1 + uncertainty-avoidance-0-1) / 2
end

to-report sex-difference? [receiver giver] ; general reporter
  report [girl?] of receiver != [girl?] of giver
end

to redraw-myself ; child procedure
  set size 1 + status
end

to set-status [new-status] ; child procedure
  set status new-status
  if status > max-status [ set status max-status ]
  if status < min-status [ set status min-status ]
end

to-report alone? ; child reporter
  report color = alone-color
end

to set-alone ; child procedure
  set color alone-color
  set favorite nobody
end

to-report visible-others
  report other children in-radius calling-distance
end

to-report my-group-members
  report other children with [ not alone? and color = [color] of myself ]
end

to-report my-group-size
  report count my-group-members
end

to-report close-children
  report other children in-radius interchild-gap
end

to-report closest-child
   report min-one-of other children [ distance myself ]
end

to-report close-group-members
  report close-children with [ color = [color]  of myself ]
end

to-report nearby-others-in-a-group                              ;; nearby is not as close as close
  report other children with [ not alone? ] in-radius intergroup-gap
end

to-report nearby-strangers
  report nearby-others-in-a-group with [ color != [color] of myself ]
end

to-report nearest-stranger
  report min-one-of nearby-strangers [distance myself]
end

to-report random-normal-in-domain [mean-v stdv-v min-v max-v]
  let v random-normal mean-v stdv-v
  while [v < min-v or v > max-v] [
    set v random-normal mean-v stdv-v
  ]
  report v
end

to record-frame ; observer procedure
  ;; create movie based on .pngs - from Katrin Meyer okt 2014
  export-view (word "view" but-first (word (100000 + ticks)) ".png")
end


;;
;; COUNTING AND MONITORING
;;

to monitor-rejection ; general procedure
  set n-rejection n-rejection + 1
end

to monitor-conferrals
  set n-conferrals-ever n-conferrals-ever + 1
end

to monitor-happy
  set n-happy-ever n-happy-ever + 1
end

to monitor-condemned-fights
   set n-fights-ever n-fights-ever + 1
   set n-fights-blamed n-fights-blamed + 1
end

to monitor-condoned-fights
   set n-fights-ever n-fights-ever + 1
   set n-fights-approved n-fights-approved + 1
end

to monitor-unhappy-but-not-fighting
  set n-unhappy-but-not-fighting-ever n-unhappy-but-not-fighting-ever + 1
end


to update-outputs ; observer procedure
  update-group-counts
  update-gender-status-gap
end

to update-group-counts ; observer procedure
  set girl-groups 0
  set boy-groups 0
  set mixed-groups 0
  let colors-in-use remove-duplicates [color] of children
  foreach colors-in-use
  [ ?1 ->
    let group-color ?1
    if group-color != alone-color
    [
      let group-members children with [color = group-color]
      let girls-present? any? group-members with [girl?]
      let boys-present? any? group-members with [not girl?]
      ifelse girls-present? and boys-present?
      [ set mixed-groups mixed-groups + 1 ]
      [
        ifelse girls-present?
        [ set girl-groups girl-groups + 1 ]
        [ set boy-groups boy-groups + 1 ]
      ]
    ]
  ]
end

to update-gender-status-gap ; observer procedure
  set avg-boy-status mean [status] of boys
  set avg-girl-status mean [status] of girls
  set gender-status-gap avg-boy-status - avg-girl-status
  if debug?
  [ show gender-status-gap
    if gender-status-gap > 1 [ show ticks ]
  ]
end

;;
;; GROUPING
;;

;; each group has its own unique color
to-report unassigned-color ; general reporter
  foreach base-colors
  [ ?1 ->
    if ?1 != alone-color
    [
      if not any? children with [color = ?1]
      [ report ?1 ]
    ]
  ]
  let try-again? true
  let random-color 0
  while [try-again?]
  [
    set random-color random-rgb-color
    set try-again? (any? children with [color = random-color])
  ]
  report random-color
end

to create-group-with-favorite ; child procedure
  set n-groups-ever n-groups-ever + 1
  let group-color unassigned-color
  set color group-color
  ask favorite [ set color group-color ]
  settle
end

;;
;; MOVEMENT
;;

to reposition-myself ; child procedure
  if any? nearby-strangers ; children from other groups within calling-distance
  [
    settle
    ask my-group-members
    [
      move-to myself
      step-off-others
    ]
  ]
  avoid-border
  step-off-others
end

to settle ; child procedure; create spatially cohesive groups when they form
  if debug? [ show "settling" ]
  while [ nearest-stranger != nobody and distance nearest-stranger < intergroup-gap ]
  [
    avoid-all-groups
  ]
  ask my-group-members
  [
    face myself
    rt random my-group-size
    forward distance myself - interchild-gap - random (sqrt(my-group-size))
  ]
end

to avoid-all-groups ; child procedure
  if debug? [ show "avoid-all-groups" ]
  let counter 0
  let max-flee-attempts 10 ;; otherwise try 5
  while [ any? nearby-others-in-a-group and counter < max-flee-attempts ]
  [
    flee
    set counter counter + 1
  ]
end

to join-group-of-favorite ; child procedure
  set color [color] of favorite
  let our-color color
  move-to favorite
  step-off-others
end

to step-back-from [another-child] ; child procedure
  face another-child
  turn-around
  while [ not can-move? interchild-gap ]
  [
    set-random-heading
  ]
  forward interchild-gap
  avoid-border
end

to step-off-others ; child procedure
  let my-color color
  let counter 0
  let max-flee-attempts 10
  while [ (any? close-children) and counter < max-flee-attempts]
  [
    move-to closest-child
    rt random 270
    fd interchild-gap
    set counter counter + 1
  ]
  ;if debug? [ show "step-off-others flee-attempts" show counter ]
end

to flee ; child procedure
  let old-patch patch-here
  let moved-away? false
  while [not moved-away?] [
    setxy random-xcor random-ycor
    avoid-border
    set moved-away? (distance old-patch > intergroup-gap)
  ]
end

to avoid-border ; child procedure
  if xcor <= min-pxcor [ set xcor min-pxcor + size ]
  if xcor >= max-pxcor [ set xcor max-pxcor - size ]
  if ycor <= min-pycor [ set ycor min-pycor + size ]
  if ycor >= max-pycor [ set ycor max-pycor - size ]
end

;; ENCAPSULATING code for manipulating hbn-table
;; table for has-been-nice; each child has a value for all others
;; every has-been-nice value starts at a neutral 0 (scale -1:1)

to setup-hbn-table [owner] ; generalized procedure
  ask owner [
    let hbnt table:make
    ask other children [ table:put hbnt who 0 ]
    set hbn-table hbnt
  ]
end

to-report get-hbn [owner other-child] ; generalized reporter
  let hbnt [hbn-table] of owner
  report table:get hbnt [who] of other-child
end

to decrease-hbn [owner other-child penalty] ; generalized procedure
  ask owner [
    let key [who] of other-child
    let value table:get hbn-table key
    set value value - penalty
    if value > max-hbn [ set value max-hbn ]
    if value < min-hbn [ set value min-hbn ]
    table:put hbn-table key value
  ]
end

to update-hbn [owner other-child target-hbn] ; generalized procedure
  ask owner
  [
    let key [who] of other-child
    let value table:get hbn-table key
    set value (1 - volatility) * value  + volatility * target-hbn
    if value > max-hbn [ set value max-hbn ]
    if value < min-hbn [ set value min-hbn ]
    table:put hbn-table key value
  ]
end

;; MAGIC NUMBER ENCAPSULATION

to-report power-distance-0-1 ; general reporter
  report power-distance / 100
end

to-report fight-tolerance-0-1 ; general reporter
  report fight-tolerance / 100
end

to-report collectivism-0-1 ; general reporter
  report 1 - (individualism / 100)
end

to-report uncertainty-avoidance-0-1 ; general reporter
  report uncertainty-avoidance / 100
end

to turn-around ; own turtle command
  right 180
end

to set-random-heading ; own turtle command
  set heading random 360 ;; or random-float 360 ??
end

to-report random-rgb-color ; own general command
  report rgb random 256 random 256 random 256
end

;;CSV
to write-children-to-csv
  csv:to-file "children.csv" [ (list xcor ycor size color) ] of children
end

to prep-csv
  if ticks >= 2000 [
    let children-list (sort (turtle-set children))
    let i (list ticks (map [[kid] -> (list [who] of kid ([status] of kid) ([shyness] of kid) ([beauty] of kid) ([power] of kid) ([kindness] of kid))] children-list))
      ;;sum [status] of girls sum [status] of boys [shyness] of children [status] of children [beauty] of children [power] of children [kindness] of children)
    set csv-list lput i csv-list
  ]
end


to write-csv
  if ticks = 5000 [
    csv:to-file "file.csv" csv-list
  ] ;;letting them have 2 breaks a day (300 ticks = 1 break) for 5 days
end

;; READING PARAMETERS FROM FILE

;; expected format of file (note quotes and brackets!):
;; [ "p name 1" "p name 2" "p name 3" "p name etc." ]
;; [ p-value-1  p-value-2  p-value-3  p-value-etc   ]
;; [ p-value-1  p-value-2  p-value-3  p-value-etc   ]

to read-parameters
  let param-names []
  let param-values []
  carefully [
    file-open parameters-filename
    set param-names file-read
    let line 0
    while [line < parameters-line-nr] [
      set line line + 1
      carefully [
        set param-values file-read
      ] [
        set line parameters-line-nr
        user-message "parameters file not long enough; used last line"
      ]
    ]
    file-close
  ] [
    user-message "parameters file not found"
  ]
  if length param-values != length param-names [
    user-message "wrong number of parameters"
  ]
  ifelse is-sensitivity-setting? param-names [
    process-parameters-sensitivity param-names param-values
  ] [
    process-parameters-general param-names param-values
  ]
end

;; format as specified above in fixed columns

to process-parameters-general [param-names param-values]
  let n-params min (list length param-names length param-values)
  foreach n-values n-params [ ?1 -> ?1 ] [ ?1 ->
    let name item ?1 param-names
    let value item ?1 param-values
    let command (word "set " name " " value)
    carefully [
      run command
    ] [
      user-message (word "error executing " command)
    ]
  ]
end

;; param-names should be exactly "parameter-name" "parameter-value" "current-seed"
;; param-values should be name of parameter (in quotes!), value of parameter, seed value
;; lines with an empty string as parameter-name and any value for parameter-value specify base settings

to-report is-sensitivity-setting? [param-names]
  report member? "parameter-name" param-names
end

to process-parameters-sensitivity [param-names param-values]
  let valid? length param-names = 3
  set valid? valid? and item 0 param-names = "parameter-name"
  set valid? valid? and item 1 param-names = "parameter-value"
  set valid? valid? and item 2 param-names = "current-seed"
  set valid? valid? and length param-values = 3
  set valid? valid? and is-string? item 0 param-values
  ifelse valid? [
    set parameter-name item 0 param-values
    set parameter-value item 1 param-values
    set current-seed item 2 param-values
    let command (word "set parameter-default-value " parameter-name)
    carefully [
      run command
    ] [
      set valid? false
    ]
    ifelse valid? [
      set command (word "set " parameter-name " " parameter-value)
      carefully [
        run command
      ] [
        user-message (word "could not set value of " parameter-name)
      ]
    ] [
      if parameter-name != "" [
        user-message (word "wrong parameter name: " parameter-name)
      ]
    ]
  ] [
    user-message "wrong format for sensitivity settings"
  ]
end

to restore-parameter-default ;; to be called at end of each run during sensitivity analysis
  if parameter-name != "" [
    let command (word "set " parameter-name " " parameter-default-value)
    run command
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
172
10
642
481
-1
-1
22.0
1
12
1
1
1
0
0
0
1
-10
10
-10
10
0
0
1
ticks
30.0

BUTTON
8
10
82
43
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
88
47
163
80
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
-1
84
171
117
n-boys
n-boys
0
40
20.0
1
1
NIL
HORIZONTAL

SLIDER
-2
119
170
152
n-girls
n-girls
0
40
20.0
1
1
NIL
HORIZONTAL

SLIDER
202
651
374
684
beauty-stdev
beauty-stdev
0
0.25
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
202
688
374
721
kindness-stdev
kindness-stdev
0
.25
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
-1
184
171
217
girl-beauty-ratio
girl-beauty-ratio
0
1
0.5
.01
1
NIL
HORIZONTAL

SLIDER
202
724
374
757
power-stdev
power-stdev
0
0.25
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
-1
254
171
287
girl-power-ratio
girl-power-ratio
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
558
653
730
686
calling-distance
calling-distance
1
6
4.0
1
1
NIL
HORIZONTAL

SLIDER
882
372
1054
405
fight-tolerance
fight-tolerance
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
0
328
172
361
individualism
individualism
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
0
364
172
397
power-distance
power-distance
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
0
399
174
432
uncertainty-avoidance
uncertainty-avoidance
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
-1
219
171
252
girl-kindness-ratio
girl-kindness-ratio
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
204
809
376
842
R&T-stdev
R&T-stdev
0
0.2
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
204
774
376
807
conferral-stdev
conferral-stdev
0
0.2
0.2
0.01
1
NIL
HORIZONTAL

PLOT
0
677
200
797
Groups
NIL
NIL
0.0
10.0
0.0
3.0
true
true
"" ""
PENS
"Boy groups" 1.0 0 -6759204 true "" "plot boy-groups"
"Girl groups" 1.0 0 -4699768 true "" "plot girl-groups"
"Mixed groups" 1.0 0 -817084 true "" "plot mixed-groups"

PLOT
866
161
1066
311
Boys status distribution
status
n
0.0
1.0
0.0
10.0
true
false
"set-plot-x-range 0 1\nset-plot-y-range 0 5\nset-histogram-num-bars 10\n\nask boys [ set-plot-pen-mode 1 ]" ""
PENS
"Boys" 1.0 0 -13791810 true "" "histogram [status] of boys"

SLIDER
381
651
553
684
volatility
volatility
0.02
0.20
0.1
0.02
1
NIL
HORIZONTAL

MONITOR
1151
686
1229
731
new groups
n-groups-ever / ticks
1
1
11

PLOT
866
10
1066
160
Girls status distribution
status
n
0.0
1.0
0.0
5.0
true
false
"set-histogram-num-bars 10\nask girls [ set-plot-pen-mode 1 ]" ""
PENS
"default" 1.0 0 -5825686 true "" "histogram [status] of girls"

MONITOR
1151
781
1259
826
Happy conferrals
n-happy-ever / ticks
2
1
11

MONITOR
1152
732
1273
777
Unhappy conferrals
n-unhappy-but-not-fighting-ever / ticks
2
1
11

MONITOR
1280
685
1337
730
Fights
n-fights-ever / ticks
2
1
11

MONITOR
1154
637
1338
682
Total happy conferrals counter
n-happy-ever
2
1
11

PLOT
644
10
849
314
Types of conferrals
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -14070903 true "" "plot (n-unhappy-but-not-fighting-ever / (ticks + 1))"
"pen-1" 1.0 0 -5298144 true "" "plot (n-fights-blamed / (ticks + 1))"
"pen-2" 1.0 0 -13840069 true "" "plot (n-happy-ever / (ticks + 1))"
"pen-3" 1.0 0 -11053225 true "" "plot (n-fights-approved / (ticks + 1))"
"pen-4" 1.0 0 -7500403 true "" "plot (n-conferrals-ever / (ticks + 1))"

MONITOR
1263
780
1361
825
Groups disband
n-group-disband / ticks
2
1
11

MONITOR
1278
730
1360
775
Leave group
(n-abandoner - n-group-disband) / ticks
2
1
11

MONITOR
1221
685
1278
730
Loners
count turtles with [color = grey]
17
1
11

SLIDER
381
689
553
722
rejection-penalty
rejection-penalty
0
0.5
0.2
0.01
1
NIL
HORIZONTAL

SWITCH
7
434
123
467
new-seed?
new-seed?
1
1
-1000

INPUTBOX
5
469
160
529
current-seed
-1.009949106E9
1
0
Number

SLIDER
665
335
837
368
personality-ratio
personality-ratio
0
1
0.25
0.05
1
NIL
HORIZONTAL

SLIDER
665
371
837
404
status-ratio
status-ratio
0
1
0.25
0.05
1
NIL
HORIZONTAL

SLIDER
666
407
838
440
relationship-ratio
relationship-ratio
0
1
0.25
0.05
1
NIL
HORIZONTAL

PLOT
1068
10
1268
160
Beauty vs status
beauty
status
0.0
1.0
0.0
1.0
false
false
"set-current-plot \"Beauty vs status\"" "clear-plot\n  set-current-plot-pen \"boys\"\n  set-plot-pen-mode 2\n  ask boys [plotxy beauty status]\n  set-current-plot-pen \"girls\"\n  set-plot-pen-mode 2\n  ask girls [plotxy beauty status]"
PENS
"boys" 1.0 0 -13791810 true "" "plot count boys"
"girls" 1.0 0 -4699768 true "" "plot count girls"

PLOT
1068
294
1268
427
Power vs status
power
status
0.0
1.0
0.0
1.0
false
false
"set-current-plot \"Power vs status\"" "clear-plot\n  set-current-plot-pen \"boys\"\n  set-plot-pen-mode 2\n  ask boys [plotxy power status]\n   set-current-plot-pen \"girls\"\n  set-plot-pen-mode 2\n  ask girls [plotxy power status]"
PENS
"boys" 1.0 0 -13791810 true "" "plot count boys"
"girls" 1.0 0 -4699768 true "" "plot count girls"

PLOT
1068
162
1268
292
Kindness vs status
kindness
status
0.0
1.0
0.0
1.0
false
false
"set-current-plot \"Kindness vs status\"" "clear-plot\n  set-current-plot-pen \"boys\"\n  set-plot-pen-mode 2\n  ask boys [plotxy kindness status]\n   set-current-plot-pen \"girls\"\n  set-plot-pen-mode 2\n  ask girls [plotxy kindness status]"
PENS
"boys" 1.0 0 -13791810 true "" "plot count boys"
"girls" 1.0 0 -4699768 true "" "plot count girls"

BUTTON
88
10
163
43
NIL
run-recess
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
380
726
552
759
fight-penalty
fight-penalty
0
0.2
0.1
0.01
1
NIL
HORIZONTAL

BUTTON
8
47
82
80
go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
29
169
179
187
boy / girl attributes
11
0.0
1

TEXTBOX
19
311
169
329
culture dimension values
11
0.0
1

TEXTBOX
690
319
840
337
sources of motivation
11
0.0
1

TEXTBOX
695
442
845
460
rough and tumble (R&T)
11
0.0
1

TEXTBOX
221
634
371
652
variance among children
11
0.0
1

TEXTBOX
591
638
741
656
vision and movement
11
0.0
1

TEXTBOX
406
633
556
651
update parameters
11
0.0
1

TEXTBOX
236
758
386
776
conferral variance
11
0.0
1

TEXTBOX
874
333
960
363
gender and status claiming
11
0.0
1

SWITCH
739
653
931
686
sex-factor-on-conferral?
sex-factor-on-conferral?
1
1
-1000

SWITCH
9
593
112
626
debug?
debug?
1
1
-1000

MONITOR
949
323
1063
368
gender status gap
gender-status-gap
5
1
11

SWITCH
9
556
143
589
record-movie?
record-movie?
1
1
-1000

TEXTBOX
1154
615
1304
633
indicators
12
0.0
1

SWITCH
844
464
976
497
R&T-offence?
R&T-offence?
1
1
-1000

SLIDER
882
409
1054
442
R&T-augmenter
R&T-augmenter
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
666
464
838
497
girl-R&T-ratio
girl-R&T-ratio
0
2
0.1
0.02
1
NIL
HORIZONTAL

SLIDER
380
762
552
795
fight-threshold
fight-threshold
0
1
0.1
0.01
1
NIL
HORIZONTAL

INPUTBOX
954
636
1146
696
recess-length-in-ticks
300.0
1
0
Number

SWITCH
983
464
1125
497
R&T-by-status?
R&T-by-status?
1
1
-1000

SLIDER
556
689
728
722
intergroup-gap
intergroup-gap
2
6
3.0
1
1
NIL
HORIZONTAL

SLIDER
556
725
728
758
interchild-gap
interchild-gap
1
4
1.5
.5
1
NIL
HORIZONTAL

INPUTBOX
393
517
640
577
parameters-filename
base-exp.parameters
1
0
String

SLIDER
393
576
640
609
parameters-line-nr
parameters-line-nr
1
100
438.0
1
1
NIL
HORIZONTAL

SLIDER
199
556
371
589
girl-shyness-ratio
girl-shyness-ratio
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
200
595
372
628
shyness-stdev
shyness-stdev
0
.25
0.25
0.01
1
NIL
HORIZONTAL

PLOT
1271
10
1471
160
Shyness vs status
shyness
status
0.0
1.0
0.0
1.0
false
false
"set-current-plot \"Shyness vs status\"" "clear-plot\n  set-current-plot-pen \"boys\"\n  set-plot-pen-mode 2\n  ask boys [plotxy shyness status]\n  set-current-plot-pen \"girls\"\n  set-plot-pen-mode 2\n   ask girls [plotxy shyness status]"
PENS
"boys" 1.0 0 -13791810 true "" "plot count boys"
"girls" 1.0 0 -7858858 true "" "plot count girls"

SLIDER
199
518
371
551
shyness-ratio
shyness-ratio
0
1
0.25
0.05
1
NIL
HORIZONTAL

SLIDER
667
501
839
534
shyness-R&T-ratio
shyness-R&T-ratio
0
1
0.9
0.05
1
NIL
HORIZONTAL

TEXTBOX
266
502
416
520
Shyness\n
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

2017 02 13
This model represents a school playground with children playing. The aim is to investigate various sources of gender differences in social status.

The current version is in review for JASSS, under the title "the status arena: A generic agent-based model about the gender status gap", by Gert Jan Hofstede, Mark Kramer and Jillian Student. See that article for more about the model.

Model documentation is available on OpenABM along with the model.


## HOW IT WORKS

Children engage in "play" that consists of mutual status exchanges according to Kemper's theory. The cumulative effect of those status conferrals becomes the agent's status. Power exchanges (fights) can occur when children think they received too little status. Children with more power win fights. The status consequences of fighting depend heavily on culture.

"Nature" is modelled as agent attributes: beauty, kindness, and power. These are inspired by Kemper's status-power theory.

"Nurture" is modelled as culture in terms of Hofstede dimensions.


## HOW TO USE IT

See the article. It shows how nurture hugely amplifies effects of nature, through the self-organization that happens in the model.

It is nice to run it very slowly at first to see what the agents actually do: group in single-colour groups, in which they exchange status until they have a reason to leave. That reason is that receive conferrals small enough to overcome their reluctance in leaving a group.

Then, varying the nature and nurture parameters can show how they reinforce one another.

## THINGS TO NOTICE

Try the personality / status / has-been-nice ratio sliders. Personality sets fighting in motion (through unkindness giver / ugliness receiver). has-been-nice speeds up the learning to have happy conferrals (by avoiding being attracted to un-nice others). Status also does this but slower (perhaps because un-nice individuals lose some status and atractiveness). As agents learn to have happy conferrals, the fraction of new groups decreases - stable groups seem to evolve.

## THINGS TO TRY

see the JASSS article

## EXTENDING THE MODEL

see the JASSS article

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

the .parameters files were used in the sensitivity analysis along with BehaviorSpace.

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

The authors would like to thank NIAS, Netherlands Institute for Advanced Study in the Humanities and Social Sciences, for supporting the research project that led to this model.

corresponding author: gertjan.hofstede@wur.nl
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

average-average-boy
false
0
Circle -7500403 true true 7 7 285
Polygon -16777216 true false 169 237 142 237 112 237 84 231 59 223 101 219 124 220 152 221 180 222 223 222 245 224 221 233 209 237 196 238
Circle -1 true false 178 90 42
Circle -16777216 true false 183 97 30
Circle -1 true false 93 87 42
Circle -16777216 true false 100 93 30

average-average-girl
false
0
Circle -7500403 true true 7 7 285
Polygon -16777216 true false 169 237 142 237 112 237 84 231 59 223 101 219 124 220 152 221 180 222 223 222 245 224 221 233 209 237 196 238
Polygon -16777216 true false 132 46 99 56 69 72 54 88 42 136 42 211 42 241 27 256 12 271 -3 271 12 226 12 196 4 163 3 136 12 91 27 46 57 31 87 16 106 7 126 2 147 1 212 7 241 24 267 46 237 61 207 46 177 46 147 46
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Circle -1 true false 178 90 42
Circle -16777216 true false 183 97 30
Circle -1 true false 93 87 42
Circle -16777216 true false 100 93 30

average-kind-boy
false
0
Circle -7500403 true true 7 8 285
Circle -1 true false 74 83 44
Polygon -16777216 true false 162 259 120 255 103 243 81 225 57 194 98 213 122 221 147 224 180 225 222 209 252 194 233 224 217 238 192 254
Circle -16777216 true false 81 90 32
Circle -1 true false 180 83 44
Circle -16777216 true false 186 90 32

average-kind-girl
false
0
Circle -7500403 true true 7 8 285
Circle -1 true false 74 83 44
Polygon -16777216 true false 162 259 120 255 103 243 81 225 57 194 98 213 122 221 147 224 180 225 222 209 252 194 233 224 217 238 192 254
Polygon -16777216 true false 134 48 101 58 71 74 56 90 44 138 44 213 44 243 29 258 14 273 -1 273 14 228 14 198 6 165 5 138 14 93 29 48 59 33 89 18 113 7 129 5 149 3 214 9 243 26 269 48 239 63 209 48 179 48 149 48
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Circle -16777216 true false 81 90 32
Circle -1 true false 180 83 44
Circle -16777216 true false 186 90 32

average-unkind-boy
false
0
Circle -7500403 true true 8 12 285
Circle -1 true false 73 84 44
Polygon -16777216 true false 154 177 132 181 103 190 78 211 57 241 95 223 129 210 155 206 180 210 219 227 249 242 230 212 214 198 180 180
Polygon -7500403 true true 60 240 165 225 180 225 180 240 255 240 210 240 165 225 195 240 105 225
Circle -16777216 true false 81 91 30
Circle -1 true false 178 84 44
Circle -16777216 true false 183 91 30

average-unkind-girl
false
0
Circle -7500403 true true 8 12 285
Circle -1 true false 73 84 44
Polygon -16777216 true false 154 177 132 181 103 190 78 211 57 241 95 223 129 210 155 206 180 210 219 227 249 242 230 212 214 198 180 180
Polygon -16777216 true false 116 50 84 54 54 69 47 125 51 176 49 212 42 242 27 257 12 272 -3 272 12 227 12 197 4 164 3 137 12 92 27 47 57 32 87 17 113 6 131 3 147 2 212 8 241 25 267 47 237 62 207 47 177 47 147 47
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Polygon -7500403 true true 60 240 165 225 180 225 180 240 255 240 210 240 165 225 195 240 105 225
Circle -16777216 true false 81 91 30
Circle -1 true false 178 84 44
Circle -16777216 true false 183 91 30

beauty-average-boy
false
0
Circle -7500403 true true 8 7 285
Polygon -16777216 true false 169 237 142 237 112 237 84 231 59 223 101 219 124 220 152 221 180 222 223 222 245 224 221 233 209 237 196 238
Circle -1 true false 72 67 66
Circle -16777216 true false 84 90 40
Circle -1 true false 165 71 66
Circle -16777216 true false 177 93 40

beauty-average-girl
false
0
Circle -7500403 true true 8 7 285
Polygon -16777216 true false 169 237 142 237 112 237 84 231 59 223 101 219 124 220 152 221 180 222 223 222 245 224 221 233 209 237 196 238
Polygon -16777216 true false 132 46 99 56 69 72 54 88 42 136 42 211 42 241 27 256 12 271 -3 271 12 226 12 196 4 163 3 136 12 91 27 46 57 31 87 16 106 7 126 2 147 1 212 7 241 24 267 46 237 61 207 46 177 46 147 46
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Circle -1 true false 72 67 66
Circle -16777216 true false 84 90 40
Circle -1 true false 165 71 66
Circle -16777216 true false 177 93 40

beauty-kind-boy
false
0
Circle -7500403 true true 7 8 285
Polygon -16777216 true false 161 258 130 255 102 242 80 224 56 193 97 212 121 220 146 223 179 224 221 208 251 193 232 223 216 237 191 253
Circle -1 true false 68 69 66
Circle -16777216 true false 77 88 46
Circle -1 true false 168 74 66
Circle -16777216 true false 178 92 46

beauty-kind-girl
false
0
Circle -7500403 true true 7 8 285
Polygon -16777216 true false 161 258 130 255 102 242 80 224 56 193 97 212 121 220 146 223 179 224 221 208 251 193 232 223 216 237 191 253
Polygon -16777216 true false 134 48 101 58 71 74 56 90 44 138 44 213 44 243 29 258 14 273 -1 273 14 228 14 198 6 165 5 138 14 93 29 48 59 33 89 18 113 7 129 5 149 3 214 9 243 26 269 48 239 63 209 48 179 48 149 48
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Circle -1 true false 68 69 66
Circle -16777216 true false 77 88 46
Circle -1 true false 168 74 66
Circle -16777216 true false 178 92 46

beauty-unkind-boy
false
0
Circle -7500403 true true 8 12 285
Circle -1 true false 71 75 64
Polygon -16777216 true false 154 177 132 181 103 190 78 211 57 241 95 223 129 210 155 206 180 210 219 227 249 242 230 212 214 198 180 180
Polygon -7500403 true true 60 240 165 225 180 225 180 240 255 240 210 240 165 225 195 240 105 225
Circle -1 true false 176 75 64
Circle -16777216 true false 84 100 36
Circle -16777216 true false 190 100 36

beauty-unkind-girl
false
0
Circle -7500403 true true 8 12 285
Circle -1 true false 71 75 64
Polygon -16777216 true false 154 177 132 181 103 190 78 211 57 241 95 223 129 210 155 206 180 210 219 227 249 242 230 212 214 198 180 180
Polygon -16777216 true false 116 50 84 54 61 79 47 125 51 176 49 212 42 242 27 257 12 272 -3 272 12 227 12 197 4 164 3 137 12 92 27 47 57 32 87 17 113 6 131 3 147 2 212 8 241 25 267 47 237 62 207 47 177 47 147 47
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Polygon -7500403 true true 60 240 165 225 180 225 180 240 255 240 210 240 165 225 195 240 105 225
Circle -1 true false 176 75 64
Circle -16777216 true false 84 100 36
Circle -16777216 true false 190 100 36

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

happy-boy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 162 259 130 258 101 246 77 227 57 194 98 213 122 221 147 224 177 224 222 209 252 194 233 224 217 238 192 254

happy-girl
false
0
Circle -7500403 true true 7 8 285
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 162 259 132 254 103 243 81 225 57 194 98 213 122 221 147 224 180 225 222 209 252 194 233 224 217 238 192 254
Polygon -16777216 true false 134 48 101 58 71 74 56 90 44 138 44 213 44 243 29 258 14 273 -1 273 14 228 14 198 6 165 5 138 14 93 29 48 59 33 89 18 113 7 129 5 149 3 214 9 243 26 269 48 239 63 209 48 179 48 149 48
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Circle -1 true false 58 71 67
Circle -16777216 true false 70 90 42

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

test-with-eyes
false
0
Circle -7500403 true true 7 8 285
Polygon -16777216 true false 162 259 120 255 103 243 81 225 57 194 98 213 122 221 147 224 180 225 222 209 252 194 233 224 217 238 192 254
Circle -1 true false 71 56 67
Circle -16777216 true false 87 80 44
Circle -1 true false 165 96 63
Circle -16777216 true false 169 114 44

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

ugly-average-boy
false
0
Circle -7500403 true true 7 7 285
Polygon -16777216 true false 169 237 142 237 112 237 84 231 59 223 101 219 124 220 152 221 180 222 223 222 245 224 221 233 209 237 196 238
Circle -1 true false 70 75 60
Circle -16777216 true false 83 90 42
Circle -1 true false 164 98 59
Circle -16777216 true false 170 112 42

ugly-average-girl
false
0
Circle -7500403 true true 7 7 285
Polygon -16777216 true false 169 237 142 237 112 237 84 231 59 223 101 219 124 220 152 221 180 222 223 222 245 224 221 233 209 237 196 238
Polygon -16777216 true false 132 46 99 56 69 72 54 88 42 136 42 211 42 241 27 256 12 271 -3 271 12 226 12 196 4 163 3 136 12 91 27 46 57 31 87 16 106 7 126 2 147 1 212 7 241 24 267 46 237 61 207 46 177 46 147 46
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Circle -1 true false 70 75 60
Circle -16777216 true false 83 90 42
Circle -1 true false 164 98 59
Circle -16777216 true false 170 112 42

ugly-kind-boy
false
0
Circle -7500403 true true 8 8 285
Polygon -16777216 true false 162 259 120 255 103 243 81 225 57 194 98 213 122 221 147 224 180 225 222 209 252 194 233 224 217 238 192 254
Circle -1 true false 82 76 54
Circle -16777216 true false 89 84 44
Circle -1 true false 165 102 56
Circle -16777216 true false 169 114 44

ugly-kind-boy2
false
0
Circle -7500403 true true 7 8 285
Circle -16777216 true false 89 83 44
Polygon -16777216 true false 162 259 120 255 103 243 81 225 57 194 98 213 122 221 147 224 180 225 222 209 252 194 233 224 217 238 192 254
Circle -16777216 true false 169 114 44

ugly-kind-girl
false
0
Circle -7500403 true true 8 8 285
Polygon -16777216 true false 162 259 120 255 103 243 81 225 57 194 98 213 122 221 147 224 180 225 222 209 252 194 233 224 217 238 192 254
Polygon -16777216 true false 134 48 101 58 71 74 56 90 44 138 44 213 44 243 29 258 14 273 -1 273 14 228 14 198 6 165 5 138 14 93 29 48 59 33 89 18 113 7 129 5 149 3 214 9 243 26 269 48 239 63 209 48 179 48 149 48
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Circle -1 true false 82 76 54
Circle -16777216 true false 89 84 44
Circle -1 true false 165 102 56
Circle -16777216 true false 169 114 44

ugly-unkind-boy
false
0
Circle -7500403 true true 8 12 285
Polygon -16777216 true false 154 177 132 181 103 190 78 211 57 241 95 223 129 210 155 206 180 210 219 227 249 242 230 212 214 198 180 180
Polygon -7500403 true true 60 240 165 225 180 225 180 240 255 240 210 240 165 225 195 240 105 225
Circle -1 true false 73 71 60
Circle -16777216 true false 85 85 44
Circle -1 true false 167 95 56
Circle -16777216 true false 171 105 44

ugly-unkind-boy2
false
0
Circle -7500403 true true 8 12 285
Circle -16777216 true false 85 85 44
Polygon -16777216 true false 154 177 132 181 103 190 78 211 57 241 95 223 129 210 155 206 180 210 219 227 249 242 230 212 214 198 180 180
Polygon -7500403 true true 60 240 165 225 180 225 180 240 255 240 210 240 165 225 195 240 105 225
Circle -16777216 true false 171 105 44

ugly-unkind-girl
false
0
Circle -7500403 true true 8 12 285
Polygon -16777216 true false 154 177 132 181 103 190 78 211 57 241 95 223 129 210 155 206 180 210 219 227 249 242 230 212 214 198 180 180
Polygon -16777216 true false 116 50 84 54 61 79 47 125 51 176 49 212 42 242 27 257 12 272 -3 272 12 227 12 197 4 164 3 137 12 92 27 47 57 32 87 17 113 6 131 3 147 2 212 8 241 25 267 47 237 62 207 47 177 47 147 47
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Polygon -7500403 true true 60 240 165 225 180 225 180 240 255 240 210 240 165 225 195 240 105 225
Circle -1 true false 73 71 60
Circle -16777216 true false 85 85 44
Circle -1 true false 167 95 56
Circle -16777216 true false 171 105 44

ugly-unkind-girl2
false
0
Circle -7500403 true true 8 12 285
Circle -16777216 true false 85 85 44
Polygon -16777216 true false 154 177 132 181 103 190 78 211 57 241 95 223 129 210 155 206 180 210 219 227 249 242 230 212 214 198 180 180
Polygon -16777216 true false 116 50 84 54 61 79 47 125 51 176 49 212 42 242 27 257 12 272 -3 272 12 227 12 197 4 164 3 137 12 92 27 47 57 32 87 17 113 6 131 3 147 2 212 8 241 25 267 47 237 62 207 47 177 47 147 47
Polygon -16777216 true false 249 78 237 66 267 51 282 81 297 126 296 170 282 231 282 247 298 278 279 284 257 274 250 257 267 186 267 141 266 127 262 103 256 89
Polygon -7500403 true true 60 240 165 225 180 225 180 240 255 240 210 240 165 225 195 240 105 225
Circle -16777216 true false 171 105 44

unkind-test
false
0
Circle -7500403 true true 6 8 285
Circle -1 true false 73 84 44
Polygon -16777216 true false 172 188 159 197 141 214 106 225 50 198 78 221 106 237 177 234 201 229 234 225 256 217 254 170 227 168 197 173
Circle -16777216 true false 81 91 30
Circle -1 true false 178 84 44
Circle -16777216 true false 183 91 30

unkind-test2
false
0
Circle -7500403 true true 10 13 285
Circle -1 true false 73 84 44
Polygon -16777216 true false 168 190 136 192 89 187 51 192 41 203 53 225 94 232 141 225 183 235 213 235 235 224 237 203 228 192 198 190
Circle -16777216 true false 81 91 30
Circle -1 true false 178 84 44
Circle -16777216 true false 183 91 30
Line -16777216 false 72 192 75 210
Line -16777216 false 92 190 96 211
Line -16777216 false 114 190 118 211
Line -16777216 false 78 212 80 225
Line -16777216 false 102 212 105 230
Rectangle -1 true false 145 191 163 211
Rectangle -1 true false 168 190 184 207
Rectangle -1 true false 190 190 208 210
Rectangle -1 true false 210 190 226 209
Rectangle -1 true false 123 190 142 210
Rectangle -1 true false 102 189 118 207
Rectangle -1 true false 81 188 98 206
Rectangle -1 true false 61 187 78 205
Rectangle -1 true false 83 208 98 233
Rectangle -1 true false 103 214 117 231
Rectangle -1 true false 124 211 141 226
Rectangle -1 true false 147 212 163 226
Rectangle -1 true false 167 214 182 230
Rectangle -1 true false 190 211 205 234
Rectangle -1 true false 210 211 225 229
Rectangle -1 true false 64 209 79 234
Polygon -1 true false 149 226 164 230 163 226 162 217 162 224
Polygon -1 true false 168 230 183 234 182 230 181 221 181 228
Polygon -1 true false 102 207 119 205 115 210 109 212
Polygon -1 true false 167 207 184 205 180 210 174 212

unkind-test3
false
0
Circle -7500403 true true 9 13 285
Circle -1 true false 73 84 44
Polygon -16777216 true false 171 190 139 192 92 187 54 192 44 203 56 225 97 232 144 225 186 235 216 235 238 224 240 203 231 192 201 190
Circle -16777216 true false 81 91 30
Circle -1 true false 178 84 44
Circle -16777216 true false 183 91 30
Line -16777216 false 72 192 75 210
Line -16777216 false 92 190 96 211
Line -16777216 false 114 190 118 211
Line -16777216 false 78 212 80 225
Line -16777216 false 102 212 105 230
Rectangle -1 true false 145 191 165 211
Rectangle -1 true false 168 190 184 207
Rectangle -1 true false 190 190 208 210
Rectangle -1 true false 210 192 224 209
Rectangle -1 true false 120 190 142 209
Rectangle -1 true false 102 189 117 210
Rectangle -1 true false 81 188 98 206
Rectangle -1 true false 61 191 78 203
Rectangle -1 true false 83 214 98 230
Rectangle -1 true false 103 214 120 229
Rectangle -1 true false 124 211 141 226
Rectangle -1 true false 146 212 163 226
Rectangle -1 true false 167 214 182 230
Rectangle -1 true false 190 211 205 234
Rectangle -1 true false 210 211 225 229
Rectangle -1 true false 64 209 79 227
Polygon -1 true false 149 226 164 230 163 226 162 217 162 224
Polygon -1 true false 168 230 183 234 182 230 181 221 181 228
Polygon -1 true false 210 226 210 221 210 235 217 235 225 229 225 222 217 220 216 219 211 222 211 229

unkindtest
false
0
Circle -7500403 true true 10 11 285
Circle -1 true false 73 84 44
Polygon -16777216 true false 154 177 132 181 103 190 78 211 57 241 95 223 129 210 155 206 180 210 219 227 249 242 230 212 214 198 180 180
Polygon -7500403 true true 60 240 165 225 180 225 180 240 255 240 210 240 165 225 195 240 105 225
Circle -16777216 true false 81 91 30
Circle -1 true false 178 84 44
Circle -16777216 true false 183 91 30
Polygon -16777216 true false 199 66 162 102 176 103 184 96 206 74 190 75 184 81
Polygon -16777216 true false 90 75 120 105 135 105 120 90 105 75 90 75 105 90

unkindtestx
false
0
Circle -7500403 true true 6 8 285
Circle -1 true false 73 84 44
Polygon -16777216 true false 173 189 160 198 142 215 107 226 51 199 79 222 107 238 178 235 202 230 235 226 257 218 255 171 228 169 198 174
Circle -16777216 true false 81 91 30
Circle -1 true false 178 84 44
Circle -16777216 true false 183 91 30
Rectangle -1 true false 160 193 180 208
Rectangle -1 true false 182 181 198 207
Rectangle -1 true false 201 173 225 204
Rectangle -1 true false 226 171 250 202

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="gender gap 20160405" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>gender-status-gap</metric>
    <enumeratedValueSet variable="girl-power-ratio">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R&amp;T-by-status?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R&amp;T-offence?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R&amp;T-augmenter">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="girl-kindness-ratio">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fight-tolerance">
      <value value="0"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="gender gap 2015 06 08 check 06 17" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="301"/>
    <metric>gender-status-gap</metric>
    <enumeratedValueSet variable="girl-power-ratio">
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="girl-kindness-ratio">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="masculinity">
      <value value="0"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rough-and-tumble-augmenter">
      <value value="0"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GSG RnT HBN 2015 07 11" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="301"/>
    <metric>gender-status-gap</metric>
    <enumeratedValueSet variable="girl-power-ratio">
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="girl-kindness-ratio">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fight-tolerance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R&amp;T-augmenter">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="has-been-nice-ratio">
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R&amp;T-offence?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiments" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>experiment-id</metric>
    <metric>girl-power-ratio</metric>
    <metric>R&amp;T-augmenter</metric>
    <metric>R&amp;T-offence?</metric>
    <metric>girl-kindness-ratio</metric>
    <metric>fight-tolerance</metric>
    <metric>relationship-ratio</metric>
    <metric>current-seed</metric>
    <metric>gender-status-gap</metric>
    <enumeratedValueSet variable="parameters-filename">
      <value value="&quot;experiments50.parameters&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="parameters-line-nr" first="1" step="1" last="4020"/>
  </experiment>
  <experiment name="base-exp" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>current-seed</metric>
    <metric>gender-status-gap</metric>
    <enumeratedValueSet variable="parameters-filename">
      <value value="&quot;base-exp.parameters&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="parameters-line-nr" first="1" step="1" last="1000"/>
  </experiment>
  <experiment name="sensitivity-runs" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>restore-parameter-default</final>
    <timeLimit steps="300"/>
    <metric>parameter-name</metric>
    <metric>parameter-value</metric>
    <metric>current-seed</metric>
    <metric>gender-status-gap</metric>
    <enumeratedValueSet variable="parameters-filename">
      <value value="&quot;sensitivity.parameters&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="parameters-line-nr" first="1" step="1" last="3150"/>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="800"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="fight-tolerance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personality-ratio">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shyness-stdev">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R&amp;T-stdev">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rejection-penalty">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beauty-stdev">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conferral-stdev">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sex-factor-on-conferral?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-boys">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="girl-shyness-ratio">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R&amp;T-offence?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kindness-stdev">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="record-movie?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parameters-line-nr">
      <value value="33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intergroup-gap">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-girls">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="status-ratio">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-seed">
      <value value="1314774984"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parameters-filename">
      <value value="&quot;base-exp.parameters&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power-stdev">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recess-length-in-ticks">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="volatility">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="girl-beauty-ratio">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="girl-power-ratio">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uncertainty-avoidance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fight-threshold">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relationship-ratio">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R&amp;T-by-status?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fight-penalty">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individualism">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="girl-R&amp;T-ratio">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R&amp;T-augmenter">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="girl-kindness-ratio">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power-distance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interchild-gap">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calling-distance">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
