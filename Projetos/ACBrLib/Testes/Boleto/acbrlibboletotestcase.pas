unit ACBrLibBoletoTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { ACBrLibBoletoTest }

  { TACBrLibBoletoTest }

  TACBrLibBoletoTest= class(TTestCase)
  published
    procedure Test_Boleto_Inicializar_Com_DiretorioInvalido;
    procedure Test_Boleto_Inicializar;
    procedure Test_Boleto_Inicializar_Ja_Inicializado;
    procedure Test_Boleto_Finalizar;
    procedure Test_Boleto_Finalizar_Ja_Finalizado;
    procedure Test_Boleto_Nome_Obtendo_LenBuffer;
    procedure Test_Boleto_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_Boleto_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_Boleto_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_Boleto_Versao;
    procedure Test_Boleto_ConfigLerValor;
    procedure Test_Boleto_ConfigGravarValor;
    procedure Test_Boleto_ConfigurarDados;
    procedure Test_Boleto_IncluirTitulos;
    procedure Test_Boleto_TotalTitulosLista;
    procedure Test_Boleto_LimparLista;
    procedure Test_Boleto_Imprimir;
    procedure Test_Boleto_GerarPDF;
    procedure Test_Boleto_GerarHTML;
    procedure Test_Boleto_GerarRemessa;
    procedure Test_Boleto_LerRetorno;
    procedure Test_Boleto_SetDiretorioArquivo;
    procedure Test_Boleto_ListaBancos;
    procedure Test_Boleto_ListaCaractTitulo;
    procedure Test_Boleto_ListaOcorrencias;
    procedure Test_Boleto_ListaOcorrenciasEX;
    procedure Test_Boleto_TamNossoNumero;
    procedure Test_Boleto_CodigosMoraAceitos;
    procedure Test_Boleto_SelecionaBanco;
    procedure Test_Boleto_MontarNossoNumero;
    procedure Test_Boleto_RetornaLinhaDigitavel;
    procedure Test_Boleto_RetornaCodigoBarras;
    procedure Test_Boleto_EnviarEmail;
  end;

implementation

uses
  Printers, OSPrinters,
  ACBrLibBoletoStaticImport, ACBrLibConsts, ACBrLibBoletoConsts, ACBrUtil;

{ TACBrLibBoletoTest }

procedure TACBrLibBoletoTest.Test_Boleto_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, Boleto_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Inicializar;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Finalizar;
begin
  AssertEquals(ErrOk, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
    Bufflen := 0;
    AssertEquals(ErrOk, Boleto_Nome(Nil, Bufflen));
    AssertEquals(Length(CLibBoletoNome), Bufflen);
end;

procedure TACBrLibBoletoTest.Test_Boleto_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibBoletoNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBoletoNome), Bufflen);
  AssertEquals(CLibBoletoNome, AStr);
end;

procedure TACBrLibBoletoTest.Test_Boleto_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibBoletoNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibBoletoNome), Bufflen);
  AssertEquals(CLibBoletoNome, AStr);

end;

procedure TACBrLibBoletoTest.Test_Boleto_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBoletoNome), Bufflen);
  AssertEquals(copy(CLibBoletoNome,1,4), AStr);
end;

procedure TACBrLibBoletoTest.Test_Boleto_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, Boleto_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibBoletoVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBoletoVersao), Bufflen);
  AssertEquals(CLibBoletoVersao, AStr);

end;

procedure TACBrLibBoletoTest.Test_Boleto_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_ConfigLerValor(CSessaoVersao, CLibBoletoNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibBoletoVersao, AStr);
end;

procedure TACBrLibBoletoTest.Test_Boleto_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, Boleto_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);

end;

procedure TACBrLibBoletoTest.Test_Boleto_ConfigurarDados;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoBoletoBancoConfig, CChaveTipoCobranca, '001'));
  AssertEquals(ErrOK, Boleto_ConfigGravar(''));

  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar Configurar Cedente', ErrOK, Boleto_ConfigurarDados('..\Cedente.ini', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, Boleto_UltimoRetorno(PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, Boleto_Finalizar());

end;

procedure TACBrLibBoletoTest.Test_Boleto_IncluirTitulos;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoBoletoBancoConfig, CChaveTipoCobranca, '001'));
  AssertEquals(ErrOK, Boleto_ConfigGravar(''));

  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','P', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, Boleto_UltimoRetorno(PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, Boleto_Finalizar());

end;

procedure TACBrLibBoletoTest.Test_Boleto_TotalTitulosLista;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  //Inclui um Titulo
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  //Contar Titulos da lista
  Bufflen := 3;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_TotalTitulosLista(PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Consultar Total Titulos Lista', '1', AStr);

  AssertEquals(ErrOK, Boleto_Finalizar());

end;

procedure TACBrLibBoletoTest.Test_Boleto_LimparLista;
begin
  AssertEquals('Erro ao limpar Lista de Titulos', ErrOK, Boleto_LimparLista());

end;

procedure TACBrLibBoletoTest.Test_Boleto_Imprimir;
var
  NomeImpressoraPDF: String;
  I: Integer;
  Bufflen: Integer;
  AStr: String;
begin
  NomeImpressoraPDF := '';
  I := 0;
  while (I < Printer.Printers.Count) and (NomeImpressoraPDF = '') do
  begin
    if (pos(' PDF', UpperCase(Printer.Printers[I])) > 0) then
      NomeImpressoraPDF := Printer.Printers[I];

    Inc( I );
  end;

  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  //Inclui um Titulo
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  AssertEquals('Erro ao tentar Imprimir Titulo', ErrOK, Boleto_Imprimir(PChar(NomeImpressoraPDF)));
  AssertEquals(ErrOK, Boleto_Finalizar());

end;

procedure TACBrLibBoletoTest.Test_Boleto_GerarPDF;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  //Inclui um Titulo
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  AssertEquals('Erro ao tentar Gerar PDF', ErrOK, Boleto_GerarPDF());
  AssertEquals(ErrOK, Boleto_Finalizar());

end;

procedure TACBrLibBoletoTest.Test_Boleto_GerarHTML;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  //Inclui um Titulo
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  AssertEquals('Erro ao tentar Gerar HTML', ErrOK, Boleto_GerarHTML());
  AssertEquals(ErrOK, Boleto_Finalizar());

end;

procedure TACBrLibBoletoTest.Test_Boleto_GerarRemessa;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  //Inclui um Titulo
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  AssertEquals('Erro ao tentar Gerar Remessa', ErrOK, Boleto_GerarRemessa(PChar(ApplicationPath),1,'Remessa.rem'));
  AssertEquals(ErrOK, Boleto_Finalizar());

end;

procedure TACBrLibBoletoTest.Test_Boleto_LerRetorno;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoBoletoDiretorioConfig, CChaveLayoutRemessa, '1'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoBoletoDiretorioConfig, CChaveLeCedenteRetorno, '1'));
  AssertEquals(ErrOK, Boleto_ConfigGravar(''));
  AssertEquals('Erro ao tentar Ler Retorno', ErrOK, Boleto_LerRetorno('C:\ACBr\Projetos\ACBrLib\Testes\Boleto','RetornoBB400.ret'));
  AssertEquals(ErrOK, Boleto_Finalizar());

end;

procedure TACBrLibBoletoTest.Test_Boleto_SetDiretorioArquivo;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Alterar Diretorio', ErrOK, Boleto_SetDiretorioArquivo('C:\Temp\','Boleto.pdf', PChar(AStr), Bufflen));
  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_ListaBancos;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Buscar Listagem de Bancos', ErrOK, Boleto_ListaBancos(PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, Boleto_UltimoRetorno(PChar(AStr), Bufflen));
  end;
  AssertEquals(ErrOK, Boleto_Finalizar());

end;

procedure TACBrLibBoletoTest.Test_Boleto_ListaCaractTitulo;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Buscar Listagem de Caracteristicas do Titulo', ErrOK, Boleto_ListaCaractTitulo(PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, Boleto_UltimoRetorno(PChar(AStr), Bufflen));
  end;
  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_ListaOcorrencias;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Buscar Listagem de Ocorrencias', ErrOK, Boleto_ListaOcorrencias(PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, Boleto_UltimoRetorno(PChar(AStr), Bufflen));
  end;
  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_ListaOcorrenciasEX;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Buscar Listagem de OcorrenciasEX', ErrOK, Boleto_ListaOcorrenciasEX(PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, Boleto_UltimoRetorno(PChar(AStr), Bufflen));
  end;
  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_TamNossoNumero;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  AssertEquals(ErrOK, Boleto_ConfigGravar(''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Consultar Tam. Nosso Numero', ErrOK,
                Boleto_TamNossoNumero('17','1212121212','123456',PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_CodigosMoraAceitos;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Consultar Cod Mora', ErrOK, Boleto_CodigosMoraAceitos(PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_SelecionaBanco;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Selecionar Banco', ErrOK, Boleto_SelecionaBanco('001',PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_MontarNossoNumero;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  AssertEquals(ErrOK, Boleto_ConfigGravar(''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Montar Nosso Numero', ErrOK, Boleto_MontarNossoNumero(0,PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_RetornaLinhaDigitavel;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  AssertEquals(ErrOK, Boleto_ConfigGravar(''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Retornar Linha Digitável', ErrOK, Boleto_RetornaLinhaDigitavel(0,PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_RetornaCodigoBarras;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  AssertEquals(ErrOK, Boleto_ConfigGravar(''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals('Erro ao tentar Retornar Codigo Barras', ErrOK, Boleto_RetornaCodigoBarras(0,PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar());
end;

procedure TACBrLibBoletoTest.Test_Boleto_EnviarEmail;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar('',''));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoEmail, CChaveNome, 'Jose'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoEmail, CChaveServidor, 'smtp.djsystem.com.br'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoEmail, CChaveUsuario, 'josemaria@djsystem.com.br'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoEmail, CChaveSenha, 'teste'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoEmail, CChaveEmailConta, 'josemaria@djsystem.com.br'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoEmail, CChavePorta, '587'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoEmail, CChaveEmailSSL, '0'));
  AssertEquals(ErrOK, Boleto_ConfigGravarValor(CSessaoEmail, CChaveEmailTLS, '1'));

  AssertEquals(ErrOK, Boleto_ConfigGravar(''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  //AssertEquals('Erro ao tentar Enviar e-mail', ErrOK, Boleto_EnviarEmail('josemaria@djsystem.com.br','Teste','Mensagem',''));

  AssertEquals(ErrOK, Boleto_Finalizar());
end;


initialization

  RegisterTest(TACBrLibBoletoTest);
end.

