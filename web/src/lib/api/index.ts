// Re-export generated client and types
export { client } from "./client/client.gen";
export * from "./client/sdk.gen";
import { Sdk } from "./client/sdk.gen";

// Initialize SDK to register it in the registry
new Sdk();
export type {
  Bangumi,
  CreateBangumi,
  Episode,
  EpisodeType,
  Platform,
  SourceType,
  Subject,
  TvShow,
} from "./client/types.gen";

// Re-export TanStack Query hooks and options
export {
  createBangumiMutation,
  getEpisodesOptions,
  getEpisodesQueryKey,
  searchBangumiOptions,
  searchBangumiQueryKey,
  searchTmdbOptions,
  searchTmdbQueryKey,
} from "./client/@tanstack/react-query.gen";
