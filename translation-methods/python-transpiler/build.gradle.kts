plugins {
    kotlin("jvm") version "1.9.21"
    application
    antlr
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    antlr("org.antlr:antlr4:4.13.0")
    testImplementation(kotlin("test"))
}

version = "0.0.0"

tasks.test {
    useJUnitPlatform()
}

tasks {

    generateGrammarSource {
        arguments = arguments + listOf("-visitor")
    }

    compileKotlin {
        dependsOn("generateGrammarSource")
    }

    compileTestKotlin {
        dependsOn("generateTestGrammarSource")
    }

    jar {
        manifest.attributes["Main-Class"] = application.mainClass

        val dependencies = configurations
            .runtimeClasspath
            .get()
            .map { zipTree(it) }

        from(dependencies)
        duplicatesStrategy = DuplicatesStrategy.EXCLUDE
    }
}

kotlin {
    jvmToolchain(10)
}

application {
    mainClass.set("MainKt")
}
