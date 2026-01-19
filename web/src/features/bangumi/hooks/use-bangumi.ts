import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  listBangumiOptions,
  listBangumiQueryKey,
  getBangumiOptions,
  getBangumiQueryKey,
  getApiBangumiSubscriptionsOptions,
  getApiBangumiSubscriptionsQueryKey,
  getApiBangumiSubscriptionsBySubscriptionIdOptions,
  getApiBangumiSubscriptionsBySubscriptionIdQueryKey,
  postApiBangumiByIdSubscribeMutation,
  patchApiBangumiSubscriptionsBySubscriptionIdMutation,
  deleteApiBangumiSubscriptionsBySubscriptionIdMutation,
  getApiMetadataSearchBgmtvOptions,
  getApiMetadataSearchTmdbOptions,
  searchMikanOptions,
  getMikanRssOptions,
  getEpisodesOptions,
  createBangumiMutation,
  updateBangumiMutation,
} from "@/lib/api";

export type MetadataSource = "Bgmtv" | "Tmdb";

// Get all subscribed bangumi (with subscription info and RSS entries)
export function useGetAllSubscriptions() {
  return useQuery({
    ...getApiBangumiSubscriptionsOptions(),
  });
}

// Alias for backward compatibility
export function useGetAllBangumi() {
  return useGetAllSubscriptions();
}

// Get subscription by ID
export function useGetSubscriptionById(subscriptionId: number) {
  return useQuery({
    ...getApiBangumiSubscriptionsBySubscriptionIdOptions({
      path: { subscriptionId },
    }),
    enabled: !!subscriptionId,
  });
}

// Subscribe to a bangumi
export function useSubscribe() {
  const queryClient = useQueryClient();
  return useMutation({
    ...postApiBangumiByIdSubscribeMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({
        queryKey: getApiBangumiSubscriptionsQueryKey(),
      });
    },
  });
}

// Update a subscription
export function useUpdateSubscription() {
  const queryClient = useQueryClient();
  return useMutation({
    ...patchApiBangumiSubscriptionsBySubscriptionIdMutation(),
    onSuccess: (_data, variables) => {
      queryClient.invalidateQueries({
        queryKey: getApiBangumiSubscriptionsQueryKey(),
      });
      queryClient.invalidateQueries({
        queryKey: getApiBangumiSubscriptionsBySubscriptionIdQueryKey({
          path: { subscriptionId: variables.path.subscriptionId },
        }),
      });
    },
  });
}

// Unsubscribe from a bangumi
export function useUnsubscribe() {
  const queryClient = useQueryClient();
  return useMutation({
    ...deleteApiBangumiSubscriptionsBySubscriptionIdMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({
        queryKey: getApiBangumiSubscriptionsQueryKey(),
      });
    },
  });
}

// Search bangumi from BGM.tv
export function useSearchBangumi(keyword: string) {
  return useQuery({
    ...getApiMetadataSearchBgmtvOptions({ query: { keyword } }),
    enabled: keyword.length > 0,
  });
}

// Search anime from TMDB
export function useSearchTmdb(keyword: string) {
  return useQuery({
    ...getApiMetadataSearchTmdbOptions({ query: { keyword } }),
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
  const queryClient = useQueryClient();
  return useMutation({
    ...createBangumiMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: listBangumiQueryKey() });
    },
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

// Get a single bangumi by ID with RSS subscriptions
export function useGetBangumiById(id: number) {
  return useQuery({
    ...getBangumiOptions({ path: { id } }),
    enabled: !!id,
  });
}

// Update a bangumi
export function useUpdateBangumi() {
  const queryClient = useQueryClient();
  return useMutation({
    ...updateBangumiMutation(),
    onSuccess: (_data, variables) => {
      // Invalidate the list query
      queryClient.invalidateQueries({ queryKey: listBangumiQueryKey() });
      // Invalidate the specific bangumi detail query
      queryClient.invalidateQueries({
        queryKey: getBangumiQueryKey({ path: { id: variables.path.id } }),
      });
    },
  });
}

