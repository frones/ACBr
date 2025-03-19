val ACBrProjectName: String by rootProject.extra
val ACBrFolder: String by rootProject.extra
val ACBrDependenciesFolder: String by rootProject.extra
val ACBrLibFolder: String by rootProject.extra
val jniLibsFolder: String by rootProject.extra
val jniLibsFolder_arm64: String by rootProject.extra
val jniLibsFolder_armeabi: String by rootProject.extra


tasks.register<Copy>("copyLibs_arm64") {
    val ProjectFromFolder =
        ACBrLibFolder + "/Fontes/" + ACBrProjectName + "/bin/Android/jniLibs/arm64-v8a"

    from(ProjectFromFolder) {
        include("**/*.so")
    }
    from(ACBrDependenciesFolder + "/OpenSSL/openssl-1.1.1d/aarch64-linux-android/Dynamic") {
        include("**/*.so")
    }
    into(jniLibsFolder_arm64)

    doFirst {
        val files = file(ProjectFromFolder).listFiles()
        if (files == null || files.isEmpty()) {
            throw GradleException("Sem ACBrLib em: " + ProjectFromFolder)
        }
    }


}

tasks.register<Copy>("copyLibs_armeabi") {
    val ProjectFromFolder =
        ACBrLibFolder + "/Fontes/" + ACBrProjectName + "/bin/Android/jniLibs/armeabi-v7a"

    from(ProjectFromFolder) {
        include("**/*.so")
    }
    from(ACBrDependenciesFolder + "/OpenSSL/openssl-1.1.1d/arm-linux-androideabi/Dynamic") {
        include("**/*.so")
    }
    into(jniLibsFolder_armeabi)

    doFirst {
        val files = file(ProjectFromFolder).listFiles()
        if (files == null || files.isEmpty()) {
            throw GradleException("Sem ACBrLib em: " + ProjectFromFolder)
        }
    }
}

tasks.register("checkACBrFolder") {
    doFirst {
        if (ACBrFolder == null || ACBrFolder.isEmpty()) {
            throw GradleException("Variável de ambiente ACBR_HOME não definida")
        }
    }
}
tasks.register<Copy>("copyLibs") {


    println("ACBR_HOME " + ACBrFolder)
    println("Copiando Bibliotecas para pasta: " + jniLibsFolder);


    dependsOn(
        tasks.getByName("checkACBrFolder"),
        tasks.getByName("copyLibs_arm64"),
        tasks.getByName("copyLibs_armeabi")
    )
}

tasks.register<Delete>("deleteJni") {
    doFirst {
        println(jniLibsFolder)
    }
    delete(jniLibsFolder)

}

tasks.getByName("preBuild").dependsOn("copyLibs")
tasks.getByName("clean").dependsOn("deleteJni")



