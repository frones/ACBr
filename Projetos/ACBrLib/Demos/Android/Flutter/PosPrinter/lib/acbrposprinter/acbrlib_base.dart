abstract class ACBrLibBase {
  int inicializar();
  int finalizar();
  int configGravar(String arquivoConfig);
  int configGravarValor(String secao, String chave, String valor);
  int configLer(String arquivoConfig);
  String configLerValor(String secao,String chave);
  String nome();
  String ultimoRetorno();
  String versao();

}