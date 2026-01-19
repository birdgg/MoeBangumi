module Web.Routers where

import Data.OpenApi (OpenApi)
import Servant.API

import Web.API.Routes qualified as API
import Web.Scalar (HTML, RawHtml)

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { api :: mode :- API.Routes
  , doc ::
      mode
        :- "docs"
          :> NamedRoutes DocRoutes
  , assets :: mode :- Raw -- Catch-all for static files with SPA fallback
  }
  deriving stock (Generic)

data DocRoutes mode = DocRoutes
  { docUI ::
      mode
        :- Get '[HTML] RawHtml
  , openApiSpec ::
      mode
        :- "openapi.json"
          :> Get '[JSON] OpenApi
  }
  deriving stock (Generic)
