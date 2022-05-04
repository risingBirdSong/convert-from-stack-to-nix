so for the idea of funding an entity, for now, funding a customer, they need a starting fund of some money to be able to get up and running in this economic ecosystem. An initial fund that is a one off funding (so it can't be spammed) so thinking of an idempotent thing in Haskell that can be called only one time. If called again then it'll return an error type message.



/deposit POST ideas 
    where and from who did the money come from? (like is it legit)
    what type of deposit is it? Income, gift, stipend, earnings on like stock? 
    admin approval? 

/withdraw post request ideas
    check if they have enough money
    reason for the withdrawal (whats the money for)
    admin approval? 

/transfer 
    check if they have enough money
    reason for the transfer (whats the money for)
    admin approval? 


mytodo i need to handle errors better, now i'm just puring a Data Constructor that represents an error State, but the problem 
is that is a status 200 code

myTodo disallow amounts smaller than a cent, currently smaller amounts like 0.001 are accepted but they shouldn't be.. this leads to a bug like in the withdrawal route which allows to withdraw small amounts that don't decrement the amount in the DB, so it's like creating money from scratch.

myTodo multiple login attempts should say something like "your're already logged on"
