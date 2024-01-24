plugins {
    kotlin("jvm") version "1.9.21"
    application
    antlr
    idea
}

group = "org.parsergen"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

version = "0.0.0"

dependencies {
    antlr("org.antlr:antlr4:4.13.0")
    implementation("com.squareup:kotlinpoet:1.15.3")
    testImplementation("org.jetbrains.kotlin:kotlin-test")
}

sourceSets {
    main {
        java {
            srcDirs("build/generated/Calc")
            srcDirs("build/generated/PyLambda")
        }
    }
}

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
    mainClass.set("Main.kt")
}

