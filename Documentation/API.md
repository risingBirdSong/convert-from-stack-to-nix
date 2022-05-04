http://localhost:3003/bank-api/customeraccount-register

POST REQUEST:
{
    "newCustomer" : 
    {"username" : "aaa",
    "email" : "aaa@aaa.com",
    "password" : "abcdef"}
}

RESPONSE {
    "email": "aaa@aaa.com",
    "bankAccount": "b94afeb4-2b24-433b-95fc-bfe9e44e4d68",
    "createdAt": "2022-03-30T05:11:18.452164111Z",
    "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOiJiOTRhZmViNC0yYjI0LTQzM2ItOTVmYy1iZmU5ZTQ0ZTRkNjgifQ.MkyhvHifEcBS_xu56eqHR2QJ_bP7pnZObGDNS6Wcua4",
    "username": "aaa",
    "balance": {
        "contents": [
            "USD",
            0,
            1
        ],
        "tag": "USD'"
    },
    "message": "thanks for signing up! please check your email to complete the registration",
    "updatedAt": "2022-03-30T05:11:18.452164111Z"
}

------------------------------------------------------------------------------------------------------

http://localhost:3003/bank-api/onetime

POST REQUEST:

{
    "customerAccountUserId" : "249bdee1-d7bc-4d97-8744-e2b299678136"
}

RESPONSE: 

    ["results" .= ("that customer account wasnt found" :: Text)]
        | (pipe operator meaning 'or')
    ["results" .= ("your account has been debited a one time 500 dollar bonus!" :: Text)]
        | 
    ["results" .= ("you've already used this bonus" :: Text)]

http://localhost:3003/bank-api/customeraccount-balanceinquiry

POST REQUEST:
{
    "balanceInquiryBankAccount" : "f22a00e9-005a-4cd7-82cb-4077059ba263"
}
after logging in, log in returns this token:
including that auth token in the headers of this POST, like:

banktoken: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOiJmMjJhMDBlOS0wMDVhLTRjZDctODJjYi00MDc3MDU5YmEyNjMifQ.gpjlRr-Mca3ObiM9GwsUBRvY-83qsYCnD-IOGlz4yeA
---

RESPONSE: 
{
    "balanceInquiryAmount": "632.87" , 
    "tag": "BalanceInquiryRes"
}


http://localhost:3003/bank-api/customeraccount-deposit

HEADERS: 
banktoken : eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOiI3YmFiYmY1Ni00NTkzLTQ4ZWEtOWYyOC1jYmU2MjViNDNmNjUifQ.p5uNmiOrEHxaaScIi-q14OLY1jBqIxV8Ql5--qzPS54

POST REQUEST:

{
   "depositAmount" : "22.43" ,
   "depositBankAccount" : "7babbf56-4593-48ea-9f28-cbe625b43f65"
}

RESPONSE:

{
    "contents": "successfully deposited 22.4343433!",
    "tag": "DepositResSuccess"
}



http://localhost:3003/bank-api/customeraccount-support

HEADERS: 
banktoken : eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOiI3YmFiYmY1Ni00NTkzLTQ4ZWEtOWYyOC1jYmU2MjViNDNmNjUifQ.p5uNmiOrEHxaaScIi-q14OLY1jBqIxV8Ql5--qzPS54

POST REQUEST: 

{
   "supportPostBankAccount" : "7babbf56-4593-48ea-9f28-cbe625b43f65", 
   "supportPostText": "i had trouble with my account (relevant details)..."
}

RESPONSE:

{
    "tag": "SupportRes",
    "supportResText": "we have received your request, we'll look into this and get back to you"
}


http://localhost:3003/bank-api/customeraccount-withdraw

HEADERS: 
banktoken : eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOiI3YmFiYmY1Ni00NTkzLTQ4ZWEtOWYyOC1jYmU2MjViNDNmNjUifQ.p5uNmiOrEHxaaScIi-q14OLY1jBqIxV8Ql5--qzPS54

POST REQUEST: 
(note, this route fails if there's a parse error on the amount or if there's insufficient funds in the account)

{
        "withdrawPostBankAccount" : "7babbf56-4593-48ea-9f28-cbe625b43f65" ,
        "withdrawPostAmount" : "0.99"
}

RESPONSE:

{
    "tag": "WithdrawResSuccess",
    "withdrawResAmount": "0.99"
}