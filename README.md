# cobalt-kiosk-data-template [![Circle CI](https://circleci.com/gh/plow-technologies/cobalt-kiosk-data-template.svg?style=shield)](https://circleci.com/gh/plow-technologies/cobalt-kiosk-data-template)

TODO: Write description here

## Installation

TODO: Write installation instructions here

## Usage

how DataTemplateEtnry Looks like:
[{"value":{"data":{"Name_of_Lease_Operator_1": "Scott","Field_Name_1": "Ling's_Oilfield", "Flowback_Water_1": 10},"address":"testAddress","company":"testCompany"},"key":{"uuid":"00000000-0000-0000-0000-000000000000","date":1418937506,"formid":1}}]


## Add forms to Cobal Kiosk Backend

cabal repl
:l Kiosk.Backend.Generator.RockShore
import Data.Traversable
traverse (insertThisFormInRockShore "127.0.0.1" "2833") currentForms


/user/key /join/get
to connect to a username
-- | /user/key/join/insert  UserCompanyJoinInsertR POST
curl -d "[\"<uuid>\",\"<user name>\",<formid>]"

to add an ipad login you use the super secret
https://onping.plowtech.net/#/users/username-manager

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
