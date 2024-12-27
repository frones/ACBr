plugins {
    alias(libs.plugins.android.library)
}

android {
    namespace = "br.com.acbr.acbrlibbal"
    compileSdk = 34

    defaultConfig {
        minSdk = 24

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
        consumerProguardFiles("consumer-rules.pro")
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_1_8
        targetCompatibility = JavaVersion.VERSION_1_8
    }
}

val ACBrLibComumJar = File("../../Comum/libs/jars/ACBrLibComum.jar")

dependencies {
    implementation(files(ACBrLibComumJar))
    implementation("net.java.dev.jna:jna:5.14.0@aar")
    implementation(libs.appcompat)
    implementation(libs.material)
    testImplementation(libs.junit)
    androidTestImplementation(libs.ext.junit)
    androidTestImplementation(libs.espresso.core)
}