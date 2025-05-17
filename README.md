# fabric


* Database-aware compiler
* HTML-aware compiler
* Type constraints
* Batteries included
  - Emails, Redis, SQLs, Queues, Storage, Testing
* Tailored the web
  - CSRF/Sessions/Websockets
* Deployment pipeline
* Built to last
  - Easy to change
  - Can't forget to handle errors
  - It's machine verified


### Programming style

* Fabric programs should consist mostly of functions that read, transform and
  store database records.
* Create new types when you need to teach the compiler about a constraint of
  your domain.

### Writing style

* When writing function documentation, imagine you are continueing the sentence
  "when you call x, you...". For example, "when you call List.map, you apply a
  function to every item of a list." Thus, the documentation for `List.map` is
  "Apply a function to every item of a list".
