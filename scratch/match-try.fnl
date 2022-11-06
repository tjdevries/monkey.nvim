(fn something [] (values 5 10))

(match-try (something)
 (5 val) (+ 5 val)
 15 (values nil :failed)
 10 ("success!")
 (catch
  (_ :failed) "twas a failure"))
