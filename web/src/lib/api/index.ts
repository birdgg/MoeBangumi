// Re-export generated client and types
export { client } from "./client/client.gen";
export * from "./client/sdk.gen";
export type {
  Bangumi,
  CreateBangumi,
  Episode,
  EpisodeType,
  Platform,
  SearchSubjectsResponse,
  SourceType,
  Subject,
  SearchTmdbResponse,
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

// Derived types for convenience
import type { SearchTmdbResponse } from "./client/types.gen";
export type TmdbTvShow = SearchTmdbResponse["results"][number];
