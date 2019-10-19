module Main where

import Data.Array (mapWithIndex, modifyAt)
import Data.Either (Either(..))
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Prelude
import React (ReactElement)
import React as React
import React.DOM as D
import React.DOM.Props as P
import ReactDOM as ReactDOM
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

renderFormField :: String -> String -> String -> (String -> Effect Unit) -> ReactElement
renderFormField id label value onChange =
  D.div
    [ P.className "form-group" ]
    [ D.label
        [ P.htmlFor id ]
        [ D.text label ]
    , D.input
        [ P.className "form-control"
        , P._type "text"
        , P._id id
        , P.value value
        , P.onChange (\event -> onChange (unsafeCoerce event).target.value)
        ]
    ]

basicInformationClass ::
  React.ReactClass
    { firstName :: String
    , lastName :: String
    , onFirstNameChange :: String -> Effect Unit
    , onLastNameChange :: String -> Effect Unit
    }
basicInformationClass =
  let
    render { firstName, lastName, onFirstNameChange, onLastNameChange } =
      D.div'
        [ D.h3'
            [ D.text "Basic Information" ]
        , renderFormField "firstName" "Firstname" firstName onFirstNameChange
        , renderFormField "lastName" "Lastname" lastName onLastNameChange
        ]
  in
    React.component "BasicInformation" \this ->
      pure
        { render: render <$> React.getProps this }

addressClass ::
  React.ReactClass
    { city :: String
    , street :: String
    , state :: String
    , onCityChange :: String -> Effect Unit
    , onStreetChange :: String -> Effect Unit
    , onStateChange :: String -> Effect Unit
    }
addressClass =
  let
    render { city, street, state, onCityChange, onStreetChange, onStateChange } =
      D.div'
        [ D.h3'
            [ D.text "Address" ]
        , renderFormField "city" "City" city onCityChange
        , renderFormField "street" "Street" street onStreetChange
        , renderFormField "state" "State" state onStateChange
        ]
  in
    React.component "Address" \this ->
      pure { render: render <$> React.getProps this }

errorClass :: React.ReactClass { error :: String }
errorClass =
  let
    render { error } =
      D.li'
        [ D.text error ]
  in
    React.component "Phone" \this ->
      pure
        { render: render <$> React.getProps this }

errorsClass :: React.ReactClass { errors :: Errors }
errorsClass =
  let
    render { errors: [] } = mempty

    render { errors } =
      D.ul
        [ P.className "alert alert-danger" ]
        ((\error -> React.createLeafElement errorClass { error }) <$> errors)
  in
    React.component "Phones" \this ->
      pure
        { render: render <$> React.getProps this }

phoneClass ::
  React.ReactClass
    { phone :: PhoneNumber
    , onPhoneChange :: String -> Effect Unit
    }
phoneClass =
  let
    render { phone: PhoneNumber { "type": type_, number }, onPhoneChange } = renderFormField (show type_) (show type_) number onPhoneChange
  in
    React.component "Phone" \this ->
      pure
        { render: render <$> React.getProps this }

phonesClass ::
  React.ReactClass
    { phones :: Array PhoneNumber
    , onPhoneChange :: Int -> String -> Effect Unit
    }
phonesClass =
  let
    render { phones, onPhoneChange } =
      D.div'
        ( [ D.h3' [ D.text "Contact Information" ] ]
            <> (mapWithIndex (\index phone -> React.createLeafElement phoneClass { phone, onPhoneChange: (onPhoneChange index) }) phones)
        )
  in
    React.component "Phones" \this ->
      pure
        { render: render <$> React.getProps this }

mainClass :: React.ReactClass {}
mainClass =
  React.component "Main" \this ->
    let
      updateFirstName (Person person) firstName = Person (person { firstName = firstName })

      updateLastName (Person person) lastName = Person (person { lastName = lastName })

      updateCity (Person person@{ homeAddress: Address address }) city = Person (person { homeAddress = Address address { city = city } })

      updateStreet (Person person@{ homeAddress: Address address }) street = Person (person { homeAddress = Address address { street = street } })

      updateState (Person person@{ homeAddress: Address address }) state = Person (person { homeAddress = Address address { state = state } })

      updatePhones i (Person person@{ phones }) s = Person (person { phones = (unsafePartial (fromJust (modifyAt i (\(PhoneNumber phone) -> PhoneNumber phone { number = s }) phones))) })

      onChange :: (Person -> String -> Person) -> String -> Effect Unit
      onChange f s =
        React.modifyState this \{ person, errors } ->
          let
            person' = f person s
          in
            case validatePerson' person' of
              Left errors' -> { person: person', errors: errors' }
              _ -> { person: person', errors: [] }

      render { person: Person { firstName, lastName, homeAddress: Address { city, street, state }, phones }, errors } =
        D.div
          [ P.className "container" ]
          [ React.createLeafElement errorsClass { errors }
          , React.createLeafElement basicInformationClass
              { firstName
              , lastName
              , onFirstNameChange: onChange updateFirstName
              , onLastNameChange: onChange updateLastName
              }
          , React.createLeafElement addressClass
              { city
              , street
              , state
              , onCityChange: onChange updateCity
              , onStreetChange: onChange updateStreet
              , onStateChange: onChange updateState
              }
          , React.createLeafElement phonesClass
              { phones
              , onPhoneChange: (\index phone -> onChange (updatePhones index) phone)
              }
          ]
    in
      pure
        { state:
          { person: examplePerson
          , errors: []
          }
        , render: render <$> React.getState this
        }

main :: Effect Unit
main =
  void do
    documentElement <- window >>= document
    let
      document = toDocument documentElement

      documentNode = toNonElementParentNode document
    divElement <- getElementById "main" documentNode
    let
      divElement' = unsafePartial $ fromJust $ divElement
    ReactDOM.render (React.createLeafElement mainClass {}) divElement'
