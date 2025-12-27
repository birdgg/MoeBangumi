import * as React from "react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Field, FieldLabel } from "@/components/ui/field";
import {
  IconMagnet,
  IconSearch,
  IconUpload,
  IconTrash,
} from "@tabler/icons-react";
import { TorrentSearchModal } from "../torrent-search-modal";

interface TorrentInputSectionProps {
  value: string;
  onChange: (value: string) => void;
  onBlur: () => void;
  torrentFile: File | null;
  onTorrentFileChange: (file: File | null) => void;
  searchKeyword: string;
}

export function TorrentInputSection({
  value,
  onChange,
  onBlur,
  torrentFile,
  onTorrentFileChange,
  searchKeyword,
}: TorrentInputSectionProps) {
  const [torrentSearchModalOpen, setTorrentSearchModalOpen] =
    React.useState(false);
  const torrentFileInputRef = React.useRef<HTMLInputElement>(null);

  const handleClear = () => {
    onChange("");
    onTorrentFileChange(null);
    if (torrentFileInputRef.current) {
      torrentFileInputRef.current.value = "";
    }
  };

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file) {
      onTorrentFileChange(file);
      onChange(file.name);
    }
  };

  const handleSelect = (magnetUrl: string) => {
    onChange(magnetUrl);
    setTorrentSearchModalOpen(false);
  };

  return (
    <Field>
      <div className="flex items-center justify-between">
        <FieldLabel>
          <IconMagnet className="size-4 text-chart-3 dark:text-chart-1" />
          种子下载
        </FieldLabel>
        <Button
          type="button"
          variant="outline"
          size="sm"
          onClick={() => setTorrentSearchModalOpen(true)}
          className="h-7 gap-1.5 border-chart-3/30 dark:border-chart-1/30 hover:bg-chart-3/10 dark:hover:bg-chart-1/20"
        >
          <IconSearch className="size-3.5" />
          搜索种子
        </Button>
      </div>
      <div className="space-y-3">
        {/* Magnet/Torrent URL Input */}
        <div className="space-y-2 p-3 rounded-lg border border-chart-3/20 dark:border-chart-1/20 bg-chart-3/5 dark:bg-chart-1/5">
          <div className="flex gap-2 items-center">
            <div className="relative flex-1">
              <IconMagnet className="absolute left-3 top-1/2 -translate-y-1/2 size-4 text-muted-foreground" />
              <Input
                value={value}
                onBlur={onBlur}
                onChange={(e) => onChange(e.target.value)}
                placeholder="粘贴磁力链接或种子URL..."
                className="pl-9"
              />
            </div>
            {/* File Upload Button */}
            <input
              ref={torrentFileInputRef}
              type="file"
              accept=".torrent"
              className="hidden"
              onChange={handleFileChange}
            />
            <Button
              type="button"
              variant="outline"
              size="icon"
              onClick={() => torrentFileInputRef.current?.click()}
              className="shrink-0 border-chart-3/30 dark:border-chart-1/30 hover:bg-chart-3/10 dark:hover:bg-chart-1/20"
              title="上传种子文件"
            >
              <IconUpload className="size-4" />
            </Button>
            {/* Clear Button */}
            {value && (
              <Button
                type="button"
                variant="outline"
                size="icon"
                onClick={handleClear}
                className="shrink-0 border-destructive/30 hover:bg-destructive/10 hover:text-destructive"
                title="清除"
              >
                <IconTrash className="size-4" />
              </Button>
            )}
          </div>
          {/* File name indicator */}
          {torrentFile && (
            <div className="flex items-center gap-2 text-xs text-muted-foreground">
              <IconUpload className="size-3" />
              <span>已选择文件: {torrentFile.name}</span>
            </div>
          )}
        </div>
        {/* Helper text */}
        <p className="text-xs text-muted-foreground">
          粘贴磁力链接或上传 .torrent 文件直接下载
        </p>
      </div>
      <TorrentSearchModal
        open={torrentSearchModalOpen}
        onOpenChange={setTorrentSearchModalOpen}
        onSelect={handleSelect}
        initialKeyword={searchKeyword}
      />
    </Field>
  );
}
