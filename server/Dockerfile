FROM maven:3.9-eclipse-temurin-17 AS build

WORKDIR /app

COPY pom.xml ./
COPY adi-common/pom.xml adi-common/
COPY adi-chat/pom.xml adi-chat/
COPY adi-admin/pom.xml adi-admin/
COPY adi-bootstrap/pom.xml adi-bootstrap/
COPY local-repo/ local-repo/

RUN mvn dependency:go-offline -B

COPY . .

RUN mvn package -DskipTests -B

FROM eclipse-temurin:17-jre-alpine

RUN apk add --no-cache musl-locales lang \

ENV LANG="zh_CN.UTF-8" \
    LANGUAGE="zh_CN:zh" \
    LC_ALL="zh_CN.UTF-8" \
    APP_VERSION="1.0.0-SNAPSHOT" \
    TZ="Asia/Shanghai" \
    APP_PROFILE="dev" \
    JAVA_OPTS="-Xms1024m -Xmx2048m -XX:+HeapDumpOnOutOfMemoryError"

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

COPY --from=build /app/adi-bootstrap/target/adi-boostrap-0.0.1-SNAPSHOT.jar /data/app/aideepin-api.jar

ENTRYPOINT ["sh","-c", "java $JAVA_OPTS -jar /data/app/aideepin-api.jar --spring.profiles.active=$APP_PROFILE"]

EXPOSE 9999
