import { defineConfig } from "@hey-api/openapi-ts";

export default defineConfig({
  input: "http://127.0.0.1:3000/docs/openapi.json",
  output: {
    path: "src/lib/api/client",
    format: "prettier",
  },
  plugins: [
    "@hey-api/typescript",
    "@hey-api/schemas",
    "@hey-api/sdk",
    "@hey-api/client-fetch",
    "@tanstack/react-query",
  ],
});
