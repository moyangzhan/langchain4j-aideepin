FROM node:22-alpine AS build

RUN npm install pnpm -g

WORKDIR /app

COPY ./package.json ./pnpm-lock.yaml ./pnpm-workspace.yaml ./

RUN pnpm install --no-frozen-lockfile

COPY . .

# Skip type-check to speed up the build and cut memory (run type-check in CI or locally via `pnpm run build`)
RUN pnpm run build-only

FROM nginx:alpine

COPY --from=build /app/dist /usr/share/nginx/html
