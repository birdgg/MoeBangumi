import { useQuery, useMutation } from "@tanstack/react-query";
import {
  searchBgmtvOptions,
  searchTmdbOptions,
  searchMikanOptions,
  getMikanRssOptions,
  getEpisodesOptions,
  createBangumiMutation,
} from "@/lib/api";

// Search bangumi from BGM.tv
export function useSearchBangumi(keyword: string) {
  return useQuery({
    ...searchBgmtvOptions({ query: { keyword } }),
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

// Search bangumi from Mikan
export function useSearchMikan(keyword: string) {
  return useQuery({
    ...searchMikanOptions({ query: { keyword } }),
    enabled: keyword.length > 0,
  });
}

// Get Mikan bangumi detail with RSS URLs
export function useMikanRss(id: string) {
  return useQuery({
    ...getMikanRssOptions({ query: { id } }),
    enabled: id.length > 0,
  });
}
