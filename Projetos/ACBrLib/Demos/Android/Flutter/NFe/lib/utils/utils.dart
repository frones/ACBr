import 'dart:io';

import 'package:archive/archive_io.dart';
import 'package:crypto/crypto.dart';
import 'package:flutter/foundation.dart' show debugPrint;
import 'package:flutter/services.dart' show rootBundle;
import 'package:path_provider/path_provider.dart';
import 'package:shared_preferences/shared_preferences.dart';

/// Caminho do arquivo ZIP dos schemas dentro dos assets do Flutter.
const String _kSchemasZipAssetPath = 'assets/schemas.zip';

/// Nome da pasta onde os schemas serão extraídos no diretório de documentos do app (pasta 'files').
/// Esta é a pasta *raiz* da extração.
const String _kSchemasRootFolderNameInFiles =
    'Schemas';

/// Chave usada no SharedPreferences para armazenar o hash da versão dos schemas extraídos.
const String _kSchemasVersionKey = 'acbr_schemas_extracted_version_hash';

/// Garante que os arquivos de schemas necessários para a ACBrLibNFe
/// estejam extraídos no diretório `files` do aplicativo.
///
/// Esta função é **idempotente**: ela só extrairá os schemas se:
/// 1. Eles ainda não tiverem sido extraídos.
/// 2. O conteúdo do arquivo `schemas.zip` nos assets tiver mudado
///    (comparado por um hash MD5).
///
/// A extração ocorre para o **diretório de documentos do aplicativo** (o que
/// corresponde à pasta `files` no Android).
///
/// Retorna uma [Future<String>] com o caminho absoluto para o diretório
/// onde os schemas foram extraídos (ex: `/data/user/0/br.com.seuapp.id/files/Schemas`).
///
/// Lança uma [Exception] em caso de falha na extração ou leitura do asset.
Future<String> extrairSchemasParaPastaFiles() async {
  try {
    final prefs = await SharedPreferences.getInstance();

    final appDir = await getAppDirByPlatform();
    final schemasRootOutputDir =
        Directory('$appDir/$_kSchemasRootFolderNameInFiles');

    // 1. Carregar o arquivo ZIP dos assets e calcular seu hash (versão)
    final byteData = await rootBundle.load(_kSchemasZipAssetPath);
    final currentZipBytes = byteData.buffer.asUint8List();
    final currentZipHash =
        md5.convert(currentZipBytes).toString(); // Calcula o hash MD5

    // 2. Obter o hash da última versão extraída das preferências
    final lastExtractedHash = prefs.getString(_kSchemasVersionKey);

    // 3. Verificar idempotência: Se o diretório raiz dos schemas existe E o hash é o mesmo, pular extração
    if (await schemasRootOutputDir.exists() &&
        lastExtractedHash == currentZipHash) {
      debugPrint(
          'Schemas já extraídos e atualizados (Hash: $currentZipHash). Pulando extração.');
      return schemasRootOutputDir.path;
    }

    // 4. Se não estiver atualizado ou não existir, proceder com a extração
    debugPrint(
        'Schemas não encontrados ou versão desatualizada. Iniciando extração para: ${schemasRootOutputDir.path}');

    // Limpar o diretório raiz dos schemas (ex: .../files/Schemas)
    // para garantir uma extração limpa (importante para atualizações)
    if (await schemasRootOutputDir.exists()) {
      await schemasRootOutputDir.delete(recursive: true);
    }
    // Cria o diretório raiz dos schemas novamente
    await schemasRootOutputDir.create(recursive: true);

    // 5. Decodificar e extrair o conteúdo do ZIP
    final archive = ZipDecoder().decodeBytes(currentZipBytes);
    for (final file in archive) {

      String relativePath = file.name;
      if (relativePath.startsWith('$_kSchemasRootFolderNameInFiles/')) {
        // Se o nome do arquivo dentro do ZIP começa com "Schemas/", remove essa parte.
        relativePath = relativePath.substring('$_kSchemasRootFolderNameInFiles/'.length);
      }
      // Se for apenas a própria pasta "Schemas" na raiz do zip, ignore-a
      if (relativePath.isEmpty && file.isDirectory) {
        continue;
      }

      final extractPath = '${schemasRootOutputDir.path}/$relativePath';

      if (file.isFile) {
        final outFile = File(extractPath);
        // Cria todos os subdiretórios necessários (ex: .../files/Schemas/NFe/)
        await outFile.parent.create(recursive: true);
        await outFile.writeAsBytes(file.content as List<int>);
      } else {
        // É um diretório
        await Directory(extractPath).create(recursive: true);
      }
    }

    // 6. Armazenar o hash da versão atual após a extração bem-sucedida
    await prefs.setString(_kSchemasVersionKey, currentZipHash);

    debugPrint(
        'Extração dos schemas completa para: ${schemasRootOutputDir.path}');
    return schemasRootOutputDir.path;
  } catch (e) {
    debugPrint('Erro crítico durante a extração dos schemas: $e');
    throw Exception(
        'Falha ao preparar os arquivos de schemas para o ACBrLibNFe: $e');
  }
}

/// Retorna o diretório de documentos do aplicativo, que é o local padrão
Future<String> getAppDirByPlatform() async {
  if (Platform.isAndroid) {
    return (await getApplicationDocumentsDirectory()).path;
  }
  return File(Platform.resolvedExecutable).parent.path;
}

/// Serve para armazenar o caminho do certificado e poder ser acessado por outras telas.
String pathCertificado = '';

/// Verifica se o arquivo de certificado PFX existe na pasta 'cert' do aplicativo.
/// Retorna true se o arquivo existir, false caso contrário.
Future<bool> doesCertificateExist() async {
  /// Nome do arquivo do certificado
  const String certificateFileName = 'certificado.pfx';
  /// Nome da pasta onde deve está o certificado
  const String certificateFolderPath = 'cert';

  try {
    final String appDir = await getAppDirByPlatform();

    final Directory certDir = Directory('$appDir/$certificateFolderPath');

    final File certificateFile = File('${certDir.path}/$certificateFileName');

    return await certificateFile.exists();
  } catch (e) {
    debugPrint('Erro ao verificar existência do certificado: $e');
    return false;
  }
}
