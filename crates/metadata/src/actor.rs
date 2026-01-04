mod handle;
mod messages;
mod runner;

pub use handle::MetadataHandle;
pub use messages::SyncStats;
use runner::MetadataActor;

use std::sync::Arc;
use std::time::Duration;

use sqlx::SqlitePool;
use tokio::sync::mpsc;

use crate::poster::PosterService;
use crate::service::MetadataService;

/// 默认同步间隔：1 小时
const DEFAULT_SYNC_INTERVAL: Duration = Duration::from_secs(60 * 60);

/// 创建 Metadata Actor 服务
///
/// # Arguments
/// * `db` - SQLite 连接池
/// * `metadata_service` - Metadata 服务 (BGM.tv + TMDB 查询)
/// * `poster_service` - Poster 下载服务
///
/// # Returns
/// `MetadataHandle` 用于提交任务
///
/// # 功能
/// - Poster 下载 (fire-and-forget)
/// - TMDB ID 自动补全
/// - 内置定时同步 (每小时一次)
pub fn create_metadata_actor(
    db: SqlitePool,
    metadata_service: Arc<MetadataService>,
    poster_service: Arc<PosterService>,
) -> MetadataHandle {
    create_metadata_actor_with_interval(db, metadata_service, poster_service, DEFAULT_SYNC_INTERVAL)
}

/// 创建 Metadata Actor 服务 (自定义同步间隔)
///
/// # Arguments
/// * `db` - SQLite 连接池
/// * `metadata_service` - Metadata 服务
/// * `poster_service` - Poster 下载服务
/// * `sync_interval` - 定时同步间隔
pub fn create_metadata_actor_with_interval(
    db: SqlitePool,
    metadata_service: Arc<MetadataService>,
    poster_service: Arc<PosterService>,
    sync_interval: Duration,
) -> MetadataHandle {
    let (sender, receiver) = mpsc::channel(100);

    let actor = MetadataActor::new(
        db,
        metadata_service,
        poster_service,
        receiver,
        sync_interval,
    );

    tokio::spawn(actor.run());

    MetadataHandle::new(sender)
}
