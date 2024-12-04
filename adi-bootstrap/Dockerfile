FROM openjdk:17

ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8" \
    APP_VERSION="1.0.0-SNAPSHOT" \
    TZ="Asia/Shanghai" \
    APP_PROFILE="dev" \
    JAVA_OPTS="-Xms1024m -Xmx2048m -XX:+HeapDumpOnOutOfMemoryError"

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

ADD ./target/adi-boostrap-0.0.1-SNAPSHOT.jar  /data/app/aideepin.jar

ENTRYPOINT ["sh","-c", "java $JAVA_OPTS -jar /data/app/aideepin.jar --spring.profiles.active=$APP_PROFILE"]

EXPOSE 9999