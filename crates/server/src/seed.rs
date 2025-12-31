use sqlx::SqlitePool;

use crate::models::CreateBangumi;
use crate::repositories::BangumiRepository;

/// Seed the database with sample bangumi data for development
pub async fn seed_bangumi(pool: &SqlitePool) -> Result<(), sqlx::Error> {
    // Check if we already have data
    let existing = BangumiRepository::get_all(pool).await?;
    if !existing.is_empty() {
        return Ok(());
    }

    tracing::debug!("Seeding database with sample bangumi...");

    let seed_data = vec![
        CreateBangumi {
            title_chinese: "葬送的芙莉莲".to_string(),
            title_japanese: Some("葬送のフリーレン".to_string()),
            title_original_chinese: "葬送的芙莉莲".to_string(),
            title_original_japanese: Some("葬送のフリーレン".to_string()),
            season: 1,
            year: 2023,
            bgmtv_id: Some(400602),
            tmdb_id: Some(209867),
            poster_url: Some("https://lain.bgm.tv/pic/cover/l/13/c5/400602_ZI8Y9.jpg".to_string()),
            air_date: "2023-09-29".to_string(),
            air_week: 5,
            total_episodes: 28,
            episode_offset: 0,
            auto_complete: true,
            save_path: "/Media/Bangumi/葬送的芙莉莲 (2023) {tmdb-209867}/Season 01".to_string(),
            source_type: crate::models::SourceType::WebRip,
            finished: true,
            platform: crate::models::Platform::Tv,
            rss_entries: vec![],
        },
        CreateBangumi {
            title_chinese: "迷宫饭".to_string(),
            title_japanese: Some("ダンジョン飯".to_string()),
            title_original_chinese: "迷宫饭".to_string(),
            title_original_japanese: Some("Dungeon Meshi".to_string()),
            season: 1,
            year: 2024,
            bgmtv_id: Some(425977),
            tmdb_id: Some(119121),
            poster_url: Some("https://lain.bgm.tv/pic/cover/l/c5/88/395378_jztpO.jpg".to_string()),
            air_date: "2024-01-04".to_string(),
            air_week: 4,
            total_episodes: 24,
            episode_offset: 0,
            auto_complete: true,
            save_path: "/Media/Bangumi/迷宫饭 (2024) {tmdb-119121}/Season 01".to_string(),
            source_type: crate::models::SourceType::WebRip,
            finished: false,
            platform: crate::models::Platform::Tv,
            rss_entries: vec![],
        },
    ];

    for data in seed_data {
        let title = data.title_chinese.clone();
        match BangumiRepository::create(pool, data).await {
            Ok(_) => tracing::debug!("Created seed bangumi: {}", title),
            Err(e) => tracing::warn!("Failed to create seed bangumi {}: {}", title, e),
        }
    }

    tracing::info!("Seed data created successfully");
    Ok(())
}
