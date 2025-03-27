plugins {
    alias(libs.plugins.android.library)
}

val ACBrComumJar = rootProject.extra["ACBrLibComumJar"] as String
android {
    namespace = "br.com.acbr.lib.consultacnpj"
    compileSdk = 35

    defaultConfig {
        minSdk = 24
        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
        consumerProguardFiles("consumer-rules.pro")
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(getDefaultProguardFile("proguard-android-optimize.txt"), "proguard-rules.pro")
        }
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_1_8
        targetCompatibility = JavaVersion.VERSION_1_8
    }
}

dependencies {
    implementation (files(ACBrComumJar))
    implementation (libs.appcompat)
    implementation (libs.material)
    implementation (libs.jna)
    testImplementation (libs.junit)
    androidTestImplementation (libs.ext.junit)
    androidTestImplementation (libs.espresso.core)
}

apply {
    from("../../Comum/comum.gradle.kts")
}