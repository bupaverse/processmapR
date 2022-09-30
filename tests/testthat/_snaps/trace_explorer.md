# test trace_explorer on eventlog with default params

    No `coverage` or `n_traces` set.
    ! Defaulting to `coverage` = 0.2 for `type` = "frequent" traces.

# test trace_explorer on eventlog with param `coverage`

    `coverage` must be a <numeric> between 0 and 1.
    x You supplied a <numeric>: -1

---

    `coverage` must be a <numeric> between 0 and 1.
    x You supplied a <numeric>: 2

---

    `coverage` must be a <numeric> between 0 and 1.
    x You supplied a <character>: "0.2"

# test trace_explorer on eventlog with param `n_traces`

    `n_traces` must be an interger-like <numeric> larger than 0.
    x You supplied a <numeric>: -1

---

    `n_traces` must be an interger-like <numeric> larger than 0.
    x You supplied a <numeric>: 0

---

    `n_traces` must be an interger-like <numeric> larger than 0.
    x You supplied a <numeric>: 1.5

---

    `n_traces` must be an interger-like <numeric> larger than 0.
    x You supplied a <character>: "1"

# test trace_explorer on eventlog with param `type`

    No `coverage` or `n_traces` set.
    ! Defaulting to `coverage` = 0.2 for `type` = "frequent" traces.

---

    No `coverage` or `n_traces` set.
    ! Defaulting to `coverage` = 0.05 for `type` = "infrequent" traces.

# test trace_explorer on activitylog with default params

    No `coverage` or `n_traces` set.
    ! Defaulting to `coverage` = 0.2 for `type` = "frequent" traces.

