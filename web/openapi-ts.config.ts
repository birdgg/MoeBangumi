import { defineConfig } from "@hey-api/openapi-ts";

export default defineConfig({
  input: "http://127.0.0.1:3000/api/openapi.json",
  output: {
    path: "src/lib/api/client",
    format: "prettier",
  },
  plugins: [
    "@hey-api/typescript",
    {
      name: "@hey-api/sdk",
      asClass: false,
      operationId: true,
    },
    {
      name: "@hey-api/client-fetch",
      runtimeConfigPath: "../config.ts",
    },
    {
      name: "@tanstack/react-query",
      queryOptions: true,
      mutationOptions: true,
    },
  ],
});
