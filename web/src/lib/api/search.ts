import { client } from "./client";
import type { SearchSubjectsResponse, TmdbSearchResponse } from "./types";

export const searchApi = {
  bangumi: (query: string) =>
    client.get<SearchSubjectsResponse>(
      `/search/bgmtv?keyword=${encodeURIComponent(query)}`
    ),
  tmdb: (keyword: string) =>
    client.get<TmdbSearchResponse>(
      `/search/tmdb?keyword=${encodeURIComponent(keyword)}`
    ),
};
