> **[🇨🇳 中文文档](README.zh-CN.md)** | English

To start all services at once, use [docker/](../../docker/README.md) in the root directory.

## Admin Web - Docker Deployment

### Usage

```bash
# Run from admin-web directory
cd docker

# Build and start
docker compose up -d --build

# View status
docker ps

# Stop
docker compose down
```

### Notes

- Builds the admin web frontend and starts an Nginx container
- Modify `proxy_pass` in `nginx/nginx.conf` to point to your backend service
- Access: `http://your-ip:80/admin/`
- This configuration only starts the admin web frontend
