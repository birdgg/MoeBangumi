import { useQuery, useMutation } from "@tanstack/react-query";
import {
  searchBangumiOptions,
  searchTmdbOptions,
  getEpisodesOptions,
  createBangumiMutation,
} from "@/lib/api";

// Search bangumi from BGM.tv
export function useSearchBangumi(keyword: string) {
  return useQuery({
    ...searchBangumiOptions({ query: { keyword } }),
    enabled: keyword.length > 0,
  });
}

// Search anime from TMDB
export function useSearchTmdb(keyword: string) {
  return useQuery({
    ...searchTmdbOptions({ query: { keyword } }),
    enabled: keyword.length > 0,
  });
}

// Get episodes by subject ID
export function useEpisodes(subjectId: number) {
  return useQuery({
    ...getEpisodesOptions({ path: { subject_id: subjectId } }),
    enabled: !!subjectId,
  });
}

// Create a new bangumi
export function useCreateBangumi() {
  return useMutation({
    ...createBangumiMutation()
  });
}
