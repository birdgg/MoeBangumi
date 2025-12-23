import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  bangumiApi,
  searchApi,
  type CreateBangumiRequest,
  type UpdateBangumiRequest,
} from "@/lib/api";

// Query keys for cache management
export const bangumiKeys = {
  all: ["bangumi"] as const,
  lists: () => [...bangumiKeys.all, "list"] as const,
  list: (filters: Record<string, unknown>) =>
    [...bangumiKeys.lists(), filters] as const,
  details: () => [...bangumiKeys.all, "detail"] as const,
  detail: (id: string | number) => [...bangumiKeys.details(), id] as const,
  search: (query: string) => [...bangumiKeys.all, "search", query] as const,
  tmdbSearch: (keyword: string) => ["tmdb", "search", keyword] as const,
};

// Search bangumi from BGM.tv
export function useSearchBangumi(query: string) {
  return useQuery({
    queryKey: bangumiKeys.search(query),
    queryFn: async () => {
      const response = await searchApi.bangumi(query);
      return response.data;
    },
    enabled: query.length > 0,
  });
}

// Search anime from TMDB
export function useSearchTmdb(keyword: string) {
  return useQuery({
    queryKey: bangumiKeys.tmdbSearch(keyword),
    queryFn: async () => {
      const response = await searchApi.tmdb(keyword);
      return response;
    },
    enabled: keyword.length > 0,
  });
}

// Get all bangumi list
export function useBangumiList() {
  return useQuery({
    queryKey: bangumiKeys.lists(),
    queryFn: () => bangumiApi.list(),
  });
}

// Get single bangumi by id
export function useBangumi(id: string | number) {
  return useQuery({
    queryKey: bangumiKeys.detail(id),
    queryFn: () => bangumiApi.get(id),
    enabled: !!id,
  });
}

// Create a new bangumi
export function useCreateBangumi() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (data: CreateBangumiRequest) => bangumiApi.create(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: bangumiKeys.lists() });
    },
  });
}

// Update a bangumi
export function useUpdateBangumi() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: ({
      id,
      ...data
    }: UpdateBangumiRequest & { id: string | number }) =>
      bangumiApi.update(id, data),
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({
        queryKey: bangumiKeys.detail(variables.id),
      });
      queryClient.invalidateQueries({ queryKey: bangumiKeys.lists() });
    },
  });
}

// Delete a bangumi
export function useDeleteBangumi() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (id: string | number) => bangumiApi.delete(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: bangumiKeys.lists() });
    },
  });
}
