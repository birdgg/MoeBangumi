import { useQuery, useMutation } from "@tanstack/react-query";
import {
  searchBangumiOptions,
  searchTmdbOptions,
  getEpisodesOptions,
  createBangumi,
  type CreateBangumi,
} from "@/lib/api";

// Search bangumi from BGM.tv
export function useSearchBangumi(keyword: string) {
  return useQuery({
    ...searchBangumiOptions({ query: { keyword } }),
    enabled: keyword.length > 0,
    select: (data) => data.data,
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
    mutationFn: async (data: CreateBangumi) => {
      const response = await createBangumi({ body: data, throwOnError: true });
      return response.data;
    },
  });
}
