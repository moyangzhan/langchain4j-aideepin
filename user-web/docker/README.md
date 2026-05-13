> **[🇨🇳 中文文档](README.zh-CN.md)** | English

## User Web - Docker Deployment

### Usage

```bash
# Run from user-web directory
cd docker

# Build and start
docker-compose up -d --build

# View status
docker ps

# Stop
docker-compose down
```

### Notes

- Builds the user web frontend and starts an Nginx container
- Modify `proxy_pass` in `nginx/nginx.conf` to point to your backend service
- Access: `http://your-ip:80/`
- This configuration only starts the user web frontend. To start all services at once, use [docker/](../../docker/README.md) in the root directory
