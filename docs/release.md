# Release 流程文档

本文档描述 `moe-bangumi` 项目的发布流程和自动化工具。

## 流程概览

```
┌─────────────────────────────────────────────────────────────┐
│                    just release 0.2.0                       │
│                  (触发 GitHub Actions)                       │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    release.yml workflow                      │
│  1. 验证版本格式                                              │
│  2. 检查 tag 是否存在                                         │
│  3. 生成 changelog (git-cliff)                               │
│  4. 构建多平台二进制                                          │
│  5. 创建 git tag                                             │
│  6. 创建 GitHub Release + 上传 assets                        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼ (tag push 触发)
┌─────────────────────────────────────────────────────────────┐
│                    docker.yml workflow                       │
│  1. 构建 linux/amd64, linux/arm64 镜像                       │
│  2. 推送到 GHCR (ghcr.io)                                    │
│  3. 推送到 Docker Hub                                        │
└─────────────────────────────────────────────────────────────┘
```

## 发布命令

### 正式发布

```bash
# 触发完整发布流程
just release 0.2.0
```

这将：
1. 触发 GitHub Actions `release.yml` workflow
2. 构建所有平台二进制
3. 生成 changelog
4. 创建 git tag `v0.2.0`
5. 创建 GitHub Release
6. 自动触发 Docker 镜像构建

### 测试发布 (Dry Run)

```bash
# 测试构建但不发布
just release-dry 0.2.0
```

Dry run 模式会：
- 执行所有构建步骤
- 生成 changelog
- **不会**创建 tag
- **不会**创建 GitHub Release
- **不会**推送 Docker 镜像

### 本地构建测试

```bash
# 在本地构建 release 版本
just release-local
```

## 版本号规范

版本号遵循 [Semantic Versioning](https://semver.org/)：

```
X.Y.Z[-suffix]

X = 主版本号 (不兼容的 API 变更)
Y = 次版本号 (向后兼容的新功能)
Z = 修订号 (向后兼容的问题修复)
suffix = 预发布标识 (可选，如 alpha, beta, rc1)
```

### 示例

| 版本 | 说明 |
|------|------|
| `0.1.0` | 首个发布版本 |
| `0.2.0` | 新增功能 |
| `0.2.1` | Bug 修复 |
| `1.0.0-alpha` | 1.0 预览版 |
| `1.0.0-rc1` | 1.0 候选版本 |

## 构建产物

### 二进制文件

| 文件名 | 平台 | 架构 |
|--------|------|------|
| `moe-linux-amd64` | Linux | x86_64 |
| `moe-linux-arm64` | Linux | ARM64 |
| `moe-macos-amd64` | macOS | x86_64 (Intel) |
| `moe-macos-arm64` | macOS | ARM64 (Apple Silicon) |
| `moe-windows-amd64.exe` | Windows | x86_64 |

### Docker 镜像

```bash
# GitHub Container Registry
docker pull ghcr.io/birdgg/moe-bangumi:0.2.0
docker pull ghcr.io/birdgg/moe-bangumi:latest

# Docker Hub
docker pull birdgg/moe-bangumi:0.2.0
docker pull birdgg/moe-bangumi:latest
```

镜像标签：
- `X.Y.Z` - 完整版本号
- `X.Y` - 主版本.次版本
- `X` - 主版本
- `latest` - 最新稳定版

## Changelog 生成

使用 [git-cliff](https://git-cliff.org/) 自动生成 changelog。

### 本地预览

```bash
# 查看未发布的变更
just changelog

# 生成完整 changelog 文件
just changelog-full
```

### Commit 规范

遵循 [Conventional Commits](https://www.conventionalcommits.org/)：

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

| Type | 说明 | Changelog 分组 |
|------|------|----------------|
| `feat` | 新功能 | Features |
| `fix` | Bug 修复 | Bug Fixes |
| `docs` | 文档更新 | Documentation |
| `perf` | 性能优化 | Performance |
| `refactor` | 重构 | Refactoring |
| `test` | 测试 | Testing |
| `chore` | 杂项 | Miscellaneous |

### 示例

```bash
git commit -m "feat(rss): add support for custom RSS feeds"
git commit -m "fix(parser): handle special characters in filenames"
git commit -m "docs(readme): update installation instructions"
```

## GitHub Secrets 配置

发布流程需要以下 secrets：

| Secret | 说明 | 用途 |
|--------|------|------|
| `DOCKERHUB_USERNAME` | Docker Hub 用户名 | 推送镜像 |
| `DOCKERHUB_TOKEN` | Docker Hub Access Token | 推送镜像认证 |

> `GITHUB_TOKEN` 由 GitHub Actions 自动提供，用于：
> - 创建 Release
> - 推送 tag
> - 推送到 GHCR

### 配置步骤

1. 进入 GitHub 仓库 Settings > Secrets and variables > Actions
2. 点击 "New repository secret"
3. 添加 `DOCKERHUB_USERNAME` 和 `DOCKERHUB_TOKEN`

## 文件结构

```
.github/workflows/
├── release.yml         # Release workflow (手动触发)
└── docker.yml          # Docker 构建 (tag 触发)

cliff.toml              # git-cliff 配置

justfile                # Release 相关命令
├── changelog           # 预览 changelog
├── changelog-full      # 生成完整 changelog
├── bump-version        # 更新版本号
├── release             # 触发发布
├── release-dry         # Dry run 发布
└── release-local       # 本地构建
```

## 发布检查清单

发布前请确认：

- [ ] 所有测试通过 (`just test`)
- [ ] 代码已合并到 main 分支
- [ ] 版本号符合语义化版本规范
- [ ] Commit messages 遵循 Conventional Commits
- [ ] GitHub Secrets 已配置

## 故障排除

### Tag 已存在

```
Error: Tag v0.2.0 already exists
```

解决方案：
1. 检查是否已发布该版本
2. 如需重新发布，先删除旧 tag：
   ```bash
   git tag -d v0.2.0
   git push origin :refs/tags/v0.2.0
   ```

### Docker 推送失败

检查 Docker Hub secrets 是否正确配置：
- `DOCKERHUB_USERNAME` - 用户名
- `DOCKERHUB_TOKEN` - Access Token (不是密码)

### 构建失败

1. 检查 GitHub Actions 日志
2. 本地测试构建：`just release-local`
3. 确保所有依赖版本兼容
