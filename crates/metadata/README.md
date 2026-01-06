# Metadata Provider

统一的元数据搜索抽象层，将 BGM.tv、TMDB 封装为标准化接口。

## 接口

### MetadataProvider Trait

```rust
#[async_trait]
pub trait MetadataProvider: Send + Sync {
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchedMetadata>, ProviderError>;
    async fn find(&self, query: &SearchQuery) -> Result<Option<SearchedMetadata>, ProviderError>;
    async fn get_detail(&self, external_id: &str) -> Result<Option<SearchedMetadata>, ProviderError>;
    async fn get_episodes(&self, external_id: &str) -> Result<Vec<Episode>, ProviderError>;
    async fn get_episode_offset(&self, external_id: &str) -> Result<i32, ProviderError>;
    fn name(&self) -> &'static str;
}
```

### 类型

| 类型 | 说明 |
|------|------|
| `SearchQuery` | 搜索参数 (`keyword`, `year`) |
| `SearchedMetadata` | 统一的搜索结果格式 |
| `MetadataSource` | 数据来源枚举 (`Bgmtv`, `Tmdb`) |
| `Episode` | 剧集信息 |
| `EpisodeType` | 剧集类型 (`Main`, `Special`, `Opening`, `Ending`) |
| `Platform` | 平台类型 (`Tv`, `Movie`, `Ova`) |

### Provider 实现

| Provider | search | find | get_detail | get_episodes | get_episode_offset |
|----------|--------|------|------------|--------------|-------------------|
| BgmtvProvider | ✓ | ✓ | ✓ | ✓ | ✓ |
| TmdbProvider | ✓ | ✓ | - | - | - |

## 使用

```rust
use metadata::{BgmtvProvider, TmdbProvider, MetadataProvider, SearchQuery};

// 搜索
let query = SearchQuery::new("葬送のフリーレン").with_year(2023);
let results = provider.search(&query).await?;

// 查找最佳匹配
let best = provider.find(&query).await?;

// 获取详情
let detail = provider.get_detail("425651").await?;

// 获取剧集
let episodes = provider.get_episodes("425651").await?;
```
