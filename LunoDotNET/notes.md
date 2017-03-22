
1. Why would you create an return an inner function over a parameter ?
when to prefer:
    let f x y = ...
and when to prefer:
    let f x = fun y -> ...      

2. Would it have been easier if there was a type that accept Result type for further parsing before declaring combinators?

3. is (symbol) always infix operator of form: parameter1 infix parameter2 ?

4. Wouldn't andThen have been much simpler if the Parser extracted a String (easily combinable with another parser success)