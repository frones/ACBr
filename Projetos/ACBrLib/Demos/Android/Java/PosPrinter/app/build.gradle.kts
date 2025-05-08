plugins {
    alias(libs.plugins.android.application)
}

android {
    namespace = "br.com.acbr.lib.posprinter.posprinter"
    compileSdk = 35

    defaultConfig {
        applicationId = "br.com.acbr.lib.posprinter.posprinter"
        minSdk = 22
        targetSdk = 35
        versionCode = 1
        versionName = "1.0"

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
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
        sourceCompatibility = JavaVersion.VERSION_11
        targetCompatibility = JavaVersion.VERSION_11
    }
}

dependencies {

    implementation(libs.appcompat)
    implementation(libs.material)
    implementation(libs.activity)
    implementation(libs.constraintlayout)
    testImplementation(libs.junit)
    androidTestImplementation(libs.ext.junit)
    androidTestImplementation(libs.espresso.core)
    //jna aar
    implementation("net.java.dev.jna:jna:5.17.0@aar")
    implementation(files("./libs/ACBrLibPosPrinter-debug.aar"))
}