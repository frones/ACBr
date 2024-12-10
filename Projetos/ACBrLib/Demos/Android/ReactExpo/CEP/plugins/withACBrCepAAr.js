const { withAndroidManifest } = require("@expo/config-plugins");

/**
 * Adiciona permissões necessárias ao AndroidManifest.xml
 */
const withACBrCepAAr = (config) => {
  return withAndroidManifest(config, (config) => {
    const manifest = config.modResults;

    // Lista de permissões necessárias
    const requiredPermissions = [
      "android.permission.INTERNET",
      "android.permission.WRITE_EXTERNAL_STORAGE",
    ];

    // Garante que a tag <uses-permission> está inicializada
    manifest.manifest["uses-permission"] =
      manifest.manifest["uses-permission"] || [];

    // Adiciona permissões que ainda não existem
    for (const permission of requiredPermissions) {
      const exists = manifest.manifest["uses-permission"].some(
        (perm) => perm.$["android:name"] === permission
      );

      if (!exists) {
        manifest.manifest["uses-permission"].push({
          $: { "android:name": permission },
        });
      }
    }

    return config;
  });
};

module.exports = withACBrCepAAr;
