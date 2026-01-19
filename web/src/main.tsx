import { StrictMode } from "react"
import { createRoot } from "react-dom/client"
import { RouterProvider, createRouter } from "@tanstack/react-router"
import { QueryClientProvider } from "@tanstack/react-query"
import { Toaster } from "@/components/ui/sonner"
import { LogStreamProvider } from "@/components/log-stream-provider"

import "./index.css"
import { queryClient } from "./lib/query-client"
import { client } from "./lib/api/client/client.gen"

// Configure API client to use relative URLs (proxied by Vite in dev)
client.setConfig({ baseUrl: "" })

// Import the generated route tree
import { routeTree } from "./routeTree.gen"

// Create a new router instance
const router = createRouter({ routeTree })

// Register the router instance for type safety
declare module "@tanstack/react-router" {
  interface Register {
    router: typeof router
  }
}

// Render the app
const rootElement = document.getElementById("root")!
if (!rootElement.innerHTML) {
  const root = createRoot(rootElement)
  root.render(
    <StrictMode>
      <QueryClientProvider client={queryClient}>
        <LogStreamProvider>
          <RouterProvider router={router} />
          <Toaster position="bottom-right" />
        </LogStreamProvider>
      </QueryClientProvider>
    </StrictMode>
  )
}
