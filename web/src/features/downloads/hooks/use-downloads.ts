import { useRef, useCallback, useMemo } from "react";
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  syncMaindata,
  pauseTorrentsMutation,
  resumeTorrentsMutation,
  deleteTorrentsMutation,
  type TorrentInfo,
  type SyncMainData,
  type SyncTorrentInfo,
} from "@/lib/api";

// Extended TorrentInfo with added_on for sorting
export interface ExtendedTorrentInfo extends TorrentInfo {
  added_on: number;
}

// Re-export for components
export type { TorrentInfo };

// Query key for sync maindata
const SYNC_MAINDATA_KEY = ["syncMaindata"] as const;

// Convert SyncTorrentInfo to ExtendedTorrentInfo (for full updates)
function syncTorrentToTorrentInfo(
  hash: string,
  sync: SyncTorrentInfo
): ExtendedTorrentInfo {
  return {
    hash,
    name: sync.name ?? "",
    state: sync.state ?? "",
    progress: sync.progress ?? 0,
    downloaded: sync.downloaded ?? 0,
    size: sync.size ?? 0,
    eta: sync.eta ?? 0,
    save_path: sync.save_path ?? "",
    added_on: sync.added_on ?? 0,
  };
}

// Apply partial update to existing torrent
function applyPartialUpdate(
  existing: ExtendedTorrentInfo,
  update: SyncTorrentInfo
): ExtendedTorrentInfo {
  return {
    ...existing,
    name: update.name ?? existing.name,
    state: update.state ?? existing.state,
    progress: update.progress ?? existing.progress,
    downloaded: update.downloaded ?? existing.downloaded,
    size: update.size ?? existing.size,
    eta: update.eta ?? existing.eta,
    save_path: update.save_path ?? existing.save_path,
    added_on: update.added_on ?? existing.added_on,
  };
}

// List all torrents with incremental sync
export function useListTorrents() {
  const ridRef = useRef<number>(0);
  const torrentsMapRef = useRef<Map<string, ExtendedTorrentInfo>>(new Map());
  const queryClient = useQueryClient();

  const queryFn = useCallback(async (): Promise<ExtendedTorrentInfo[]> => {
    const { data } = await syncMaindata({
      query: { rid: ridRef.current },
      throwOnError: true,
    });

    const syncData = data as SyncMainData;

    // Update rid for next request
    ridRef.current = syncData.rid;

    // Handle full update
    if (syncData.full_update || torrentsMapRef.current.size === 0) {
      torrentsMapRef.current.clear();
      if (syncData.torrents) {
        for (const [hash, torrent] of Object.entries(syncData.torrents)) {
          torrentsMapRef.current.set(
            hash,
            syncTorrentToTorrentInfo(hash, torrent)
          );
        }
      }
    } else {
      // Handle incremental update
      // Apply changes
      if (syncData.torrents) {
        for (const [hash, update] of Object.entries(syncData.torrents)) {
          const existing = torrentsMapRef.current.get(hash);
          if (existing) {
            torrentsMapRef.current.set(hash, applyPartialUpdate(existing, update));
          } else {
            // New torrent
            torrentsMapRef.current.set(
              hash,
              syncTorrentToTorrentInfo(hash, update)
            );
          }
        }
      }

      // Remove deleted torrents
      if (syncData.torrents_removed) {
        for (const hash of syncData.torrents_removed) {
          torrentsMapRef.current.delete(hash);
        }
      }
    }

    // Helper to check if torrent is actively downloading
    const isActivelyDownloading = (state: string) => {
      return (
        state === "downloading" ||
        state === "forcedDL" ||
        state === "metaDL" ||
        state === "allocating" ||
        state === "stalledDL"
      );
    };

    // Sort by: 1) actively downloading first, 2) then by added_on descending
    return Array.from(torrentsMapRef.current.values()).sort((a, b) => {
      const aDownloading = isActivelyDownloading(a.state);
      const bDownloading = isActivelyDownloading(b.state);

      // If one is downloading and the other isn't, downloading comes first
      if (aDownloading && !bDownloading) return -1;
      if (!aDownloading && bDownloading) return 1;

      // Otherwise sort by added_on descending (newest first)
      return b.added_on - a.added_on;
    });
  }, []);

  const query = useQuery({
    queryKey: SYNC_MAINDATA_KEY,
    queryFn,
    refetchInterval: 60000, // Poll every 1 minute
    staleTime: 30000,
  });

  // Reset state on error to allow fresh sync
  const reset = useCallback(() => {
    ridRef.current = 0;
    torrentsMapRef.current.clear();
    queryClient.invalidateQueries({ queryKey: SYNC_MAINDATA_KEY });
  }, [queryClient]);

  return useMemo(
    () => ({
      ...query,
      reset,
    }),
    [query, reset]
  );
}

// Pause torrents mutation
export function usePauseTorrents() {
  const queryClient = useQueryClient();
  return useMutation({
    ...pauseTorrentsMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: SYNC_MAINDATA_KEY });
    },
  });
}

// Resume torrents mutation
export function useResumeTorrents() {
  const queryClient = useQueryClient();
  return useMutation({
    ...resumeTorrentsMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: SYNC_MAINDATA_KEY });
    },
  });
}

// Delete torrents mutation
export function useDeleteTorrents() {
  const queryClient = useQueryClient();
  return useMutation({
    ...deleteTorrentsMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: SYNC_MAINDATA_KEY });
    },
  });
}
