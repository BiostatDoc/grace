# print methods are stable

    Code
      print(design)
    Message
      
      -- GRACE design ----------------------------------------------------------------
      * Arms (ratio): A (1), B (1)
      * Total participants: 20
      * Run-in: 10 participants, method tilted
      Checkpoints:
    Output
        n   type     target
       10 run-in   A=5, B=5
       15 forced   A=8, B=7
       20  final A=10, B=10
    Message
      Balance variables:
    Output
       variable        type weight
            sex categorical  0.333
           race categorical  0.333
            age  continuous  0.333

---

    Code
      print(assignment)
    Message
      
      -- GRACE assignment ------------------------------------------------------------
      * Participant: P100
      * Arm: B
      * Phase: msb-biased
      Group sizes:
    Output
       arm n  pct target_next target_final
         A 6 46.2           8           10
         B 7 53.8           7           10

---

    Code
      print(status)
    Message
      
      -- GRACE status ----------------------------------------------------------------
      * Participants randomized: 12
      * Filters: from = 2; vars = sex, age
      Group sizes:
    Output
       arm n pct target_next target_final
         A 6  50           8           10
         B 6  50           7           10
    Message
      Plots are in `$imbalance_plot` and `$smd_plot`.

