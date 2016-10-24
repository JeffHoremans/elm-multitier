module Multitier.Error exposing (Error(..))

import Http

type Error = NetworkError Http.Error | ServerError String | MultitierError String
