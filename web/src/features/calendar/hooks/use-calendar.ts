import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  getCalendarOptions,
  getCalendarQueryKey,
  refreshCalendarMutation,
} from "@/lib/api";
import { toast } from "sonner";

export function useCalendar() {
  return useQuery({
    ...getCalendarOptions(),
  });
}

export function useRefreshCalendar() {
  const queryClient = useQueryClient();
  return useMutation({
    ...refreshCalendarMutation(),
    onSuccess: (data) => {
      // Update the calendar query cache with the new data
      queryClient.setQueryData(getCalendarQueryKey(), data);
      toast.success("日历数据已刷新");
    },
    onError: () => {
      toast.error("刷新失败，请稍后重试");
    },
  });
}
