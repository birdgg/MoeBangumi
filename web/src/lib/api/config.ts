import type { CreateClientConfig } from "./client/client";

export const createClientConfig: CreateClientConfig = (config) => ({
  ...config,
  baseUrl: "",
});
