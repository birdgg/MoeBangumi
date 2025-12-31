use sqlx::SqlitePool;

use crate::models::{CreateBangumi, CreateMetadata, Platform, SourceType};
use crate::repositories::BangumiRepository;

/// Seed the database with sample bangumi data for development
pub async fn seed_bangumi(_pool: &SqlitePool) -> Result<(), sqlx::Error> {
    // Check if we already have data
    let existing = BangumiRepository::get_all(_pool).await?;
    if !existing.is_empty() {
        return Ok(());
    }

    tracing::debug!("Seeding database with sample bangumi...");

    // Note: Seed data would need to be created via BangumiService now,
    // since it requires metadata creation. For now, just log a message.
    // In production, use the API to create bangumi with proper metadata.

    let _seed_data = vec![
        CreateBangumi {
            metadata_id: None,
            metadata: Some(CreateMetadata {
                mikan_id: None,
                bgmtv_id: Some(400602),
                tmdb_id: Some(209867),
                title_chinese: "葬送的芙莉莲".to_string(),
                title_japanese: Some("葬送のフリーレン".to_string()),
                title_original_chinese: Some("葬送的芙莉莲".to_string()),
                title_original_japanese: Some("葬送のフリーレン".to_string()),
                season: 1,
                year: 2023,
                platform: Platform::Tv,
                total_episodes: 28,
                poster_url: Some("https://lain.bgm.tv/pic/cover/l/13/c5/400602_ZI8Y9.jpg".to_string()),
                air_date: Some("2023-09-29".to_string()),
                air_week: 5,
                finished: true,
            }),
            episode_offset: 0,
            auto_complete: true,
            source_type: SourceType::WebRip,
            rss_entries: vec![],
            save_path: String::new(),
        },
        CreateBangumi {
            metadata_id: None,
            metadata: Some(CreateMetadata {
                mikan_id: None,
                bgmtv_id: Some(425977),
                tmdb_id: Some(119121),
                title_chinese: "迷宫饭".to_string(),
                title_japanese: Some("ダンジョン飯".to_string()),
                title_original_chinese: Some("迷宫饭".to_string()),
                title_original_japanese: Some("Dungeon Meshi".to_string()),
                season: 1,
                year: 2024,
                platform: Platform::Tv,
                total_episodes: 24,
                poster_url: Some("https://lain.bgm.tv/pic/cover/l/c5/88/395378_jztpO.jpg".to_string()),
                air_date: Some("2024-01-04".to_string()),
                air_week: 4,
                finished: false,
            }),
            episode_offset: 0,
            auto_complete: true,
            source_type: SourceType::WebRip,
            rss_entries: vec![],
            save_path: String::new(),
        },
    ];

    // Seeding now requires BangumiService with MetadataService dependency.
    // This is a placeholder - actual seeding should be done via API or
    // a dedicated seeding command that has access to services.
    tracing::info!("Seed data definition available (requires service layer for creation)");
    Ok(())
}
