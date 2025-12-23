import { defineConfig } from "@hey-api/openapi-ts";

export default defineConfig({
  input: "http://127.0.0.1:3000/api/openapi.json",
  output: {
    path: "src/lib/api/client",
    format: "prettier",
  },
  plugins: [
    "@hey-api/typescript",
    "@hey-api/schemas",
    {
      name: "@hey-api/sdk",
      instance: true
    },
    {
      name: "@hey-api/client-fetch",
      runtimeConfigPath: "../config.ts",
    },
    "@tanstack/react-query",
  ],
});
