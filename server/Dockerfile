FROM maven:3.9-eclipse-temurin-17 AS build

WORKDIR /app

COPY pom.xml ./
COPY adi-common/pom.xml adi-common/
COPY adi-chat/pom.xml adi-chat/
COPY adi-admin/pom.xml adi-admin/
COPY adi-bootstrap/pom.xml adi-bootstrap/
COPY local-repo/ local-repo/

RUN --mount=type=cache,target=/root/.m2/repository \
    mvn package -DskipTests -B

COPY . .

RUN --mount=type=cache,target=/root/.m2/repository \
    mvn package -DskipTests -B

FROM eclipse-temurin:17-jre

RUN apt-get update && apt-get install -y --no-install-recommends locales && rm -rf /var/lib/apt/lists/*

ENV LANG="zh_CN.UTF-8" \
    LANGUAGE="zh_CN:zh" \
    LC_ALL="zh_CN.UTF-8" \
    APP_VERSION="1.0.0-SNAPSHOT" \
    TZ="Asia/Shanghai" \
    APP_PROFILE="dev" \
    JAVA_OPTS="-Xms512m -Xmx1536m -XX:+HeapDumpOnOutOfMemoryError"

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

COPY --from=build /app/adi-bootstrap/target/adi-bootstrap-0.0.1-SNAPSHOT.jar /data/app/aideepin-api.jar

ENTRYPOINT ["sh","-c", "java $JAVA_OPTS -jar /data/app/aideepin-api.jar --spring.profiles.active=$APP_PROFILE"]

EXPOSE 9999
