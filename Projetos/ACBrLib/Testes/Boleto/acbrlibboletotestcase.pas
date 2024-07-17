{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrLibBoletoTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibBoletoNome = 'ACBrLibBoleto';

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
    procedure Test_Boleto_GerarRemessaStream;
    procedure Test_Boleto_LerRetorno;
    procedure Test_Boleto_LerRetornoStream;
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
  ACBrLibBoletoStaticImportMT, ACBrLibConsts, ACBrLibBoletoConsts, ACBrUtil, Dialogs
  ,synacode;

{ TACBrLibBoletoTest }

procedure TACBrLibBoletoTest.Test_Boleto_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin

  try
  //  Boleto_Finalizar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, Boleto_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TACBrLibBoletoTest.Test_Boleto_Inicializar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Finalizar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, Boleto_Finalizar(Handle))
end;

procedure TACBrLibBoletoTest.Test_Boleto_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin

  try
   AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
   AssertEquals(ErrOk, Boleto_Finalizar(Handle));
   //AssertEquals(ErrOk, Boleto_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TACBrLibBoletoTest.Test_Boleto_Nome_Obtendo_LenBuffer;
var
  Handle: THandle;
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
    AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
    Bufflen := 0;
    AssertEquals(ErrOk, Boleto_Nome(Handle, Nil, Bufflen));
    AssertEquals(Length(CLibBoletoNome), Bufflen);
    AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Nome_Lendo_Buffer_Tamanho_Identico;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
  Bufflen := Length(CLibBoletoNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBoletoNome), Bufflen);
  AssertEquals(CLibBoletoNome, AStr);
  AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Nome_Lendo_Buffer_Tamanho_Maior;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
  Bufflen := Length(CLibBoletoNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibBoletoNome), Bufflen);
  AssertEquals(CLibBoletoNome, AStr);
  AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Nome_Lendo_Buffer_Tamanho_Menor;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBoletoNome), Bufflen);
  AssertEquals(copy(CLibBoletoNome,1,4), AStr);
  AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Versao;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, Boleto_Versao(Handle, Nil, Bufflen));
  Assert(Bufflen > 0);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_Versao(Handle, PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert(AStr <> '');
  AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_ConfigLerValor;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_ConfigLerValor(Handle, CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
  AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_ConfigGravarValor;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, Boleto_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Boleto_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_ConfigurarDados;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoBoletoBancoConfig, CChaveTipoCobranca, '001'));
  //AssertEquals(ErrOK, Boleto_ConfigGravar(Handle,''));
  //
  //// Obtendo o Tamanho //
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //
  //AssertEquals('Erro ao tentar Configurar Cedente', ErrOK, Boleto_ConfigurarDados(Handle, '..\Cedente.ini', PChar(AStr), Bufflen));
  //
  //if Bufflen > 255 then
  //begin
  //  AStr := Space(Bufflen);
  //  AssertEquals(ErrOK, Boleto_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  //end;

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));

end;

procedure TACBrLibBoletoTest.Test_Boleto_IncluirTitulos;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoBoletoBancoConfig, CChaveTipoCobranca, '001'));
  //AssertEquals(ErrOK, Boleto_ConfigGravar(Handle, ''));
  //
  //// Obtendo o Tamanho //
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle, '..\Titulo.ini','P', PChar(AStr), Bufflen));
  //
  //if Bufflen > 255 then
  //begin
  //  AStr := Space(Bufflen);
  //  AssertEquals(ErrOK, Boleto_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  //end;

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));

end;

procedure TACBrLibBoletoTest.Test_Boleto_TotalTitulosLista;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle,'',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  ////Inclui um Titulo
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle, '..\Titulo.ini','', PChar(AStr), Bufflen));
  ////Contar Titulos da lista
  //Bufflen := 3;
  //AStr := Space(Bufflen);
  //AssertEquals(ErrOk, Boleto_TotalTitulosLista(Handle, PChar(AStr), Bufflen));
  //AStr := copy(AStr,1,Bufflen);
  //AssertEquals('Erro ao Consultar Total Titulos Lista', '1', AStr);

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));

end;

procedure TACBrLibBoletoTest.Test_Boleto_LimparLista;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle,'',''));
  AssertEquals('Erro ao limpar Lista de Titulos', ErrOK, Boleto_LimparLista(Handle));
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_Imprimir;
var
  Handle: THandle;
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

  AssertEquals(ErrOk, Boleto_Inicializar(Handle,'',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  ////Inclui um Titulo
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle,'..\Titulo.ini','', PChar(AStr), Bufflen));
  //AssertEquals('Erro ao tentar Imprimir Titulo', ErrOK, Boleto_Imprimir(Handle, PChar(NomeImpressoraPDF)));
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));

end;

procedure TACBrLibBoletoTest.Test_Boleto_GerarPDF;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  ////Inclui um Titulo
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle, '..\Titulo.ini','', PChar(AStr), Bufflen));
  //AssertEquals('Erro ao tentar Gerar PDF', ErrOK, Boleto_GerarPDF(Handle));
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));

end;

procedure TACBrLibBoletoTest.Test_Boleto_GerarHTML;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  ////Inclui um Titulo
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle, '..\Titulo.ini','', PChar(AStr), Bufflen));
  //AssertEquals('Erro ao tentar Gerar HTML', ErrOK, Boleto_GerarHTML(Handle));
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));

end;

procedure TACBrLibBoletoTest.Test_Boleto_GerarRemessa;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  ////Inclui um Titulo
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle, '..\Titulo.ini','', PChar(AStr), Bufflen));
  //AssertEquals('Erro ao tentar Gerar Remessa', ErrOK, Boleto_GerarRemessa(Handle, PChar(ApplicationPath),1,'Remessa.rem'));
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));

end;

procedure TACBrLibBoletoTest.Test_Boleto_GerarRemessaStream;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle, '..\Titulo.ini',''));
  //AssertEquals('Erro ao tentar Gerar Remessa', ErrOK, Boleto_GerarRemessaStream(Handle, PChar(ApplicationPath), 1,'Remessa.rem', PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_LerRetorno;
var
  Handle: THandle;
begin

  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoBoletoDiretorioConfig, CChaveLayoutRemessa, '1'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoBoletoDiretorioConfig, CChaveLeCedenteRetorno, '1'));
  //AssertEquals(ErrOK, Boleto_ConfigGravar(Handle, ''));
  //AssertEquals('Erro ao tentar Ler Retorno', ErrOK, Boleto_LerRetorno(Handle, 'C:\Projeto ACBR\ACBR\Projetos\ACBrLib\Testes\Boleto','RetornoBB400.ret'));
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));

end;

procedure TACBrLibBoletoTest.Test_Boleto_LerRetornoStream;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  Stream: TStringStream;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle,'',''));
  //Resposta:= '';
  //Tamanho:= 0;
  //
  //Stream := TStringStream.Create;
  //Stream.LoadFromFile('C:/Temp/CNAB400.ret');
  //
  //AssertEquals('Erro ao Ler Retorno Stream', ErrOK,
  //Boleto_LerRetornoStream(Handle,PChar(EncodeBase64(Stream.DataString)), Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOk, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_SetDiretorioArquivo;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Alterar Diretorio', ErrOK, Boleto_SetDiretorioArquivo(Handle, 'C:\Temp\','Boleto.pdf', PChar(AStr), Bufflen));
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_ListaBancos;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Buscar Listagem de Bancos', ErrOK, Boleto_ListaBancos(Handle, PChar(AStr), Bufflen));
  //
  //if Bufflen > 255 then
  //begin
  //  AStr := Space(Bufflen);
  //  AssertEquals(ErrOK, Boleto_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  //end;
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));

end;

procedure TACBrLibBoletoTest.Test_Boleto_ListaCaractTitulo;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Buscar Listagem de Caracteristicas do Titulo', ErrOK, Boleto_ListaCaractTitulo(Handle, PChar(AStr), Bufflen));
  //
  //if Bufflen > 255 then
  //begin
  //  AStr := Space(Bufflen);
  //  AssertEquals(ErrOK, Boleto_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  //end;
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_ListaOcorrencias;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Buscar Listagem de Ocorrencias', ErrOK, Boleto_ListaOcorrencias(Handle, PChar(AStr), Bufflen));
  //
  //if Bufflen > 255 then
  //begin
  //  AStr := Space(Bufflen);
  //  AssertEquals(ErrOK, Boleto_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  //end;
  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_ListaOcorrenciasEX;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Buscar Listagem de OcorrenciasEX', ErrOK, Boleto_ListaOcorrenciasEX(Handle, PChar(AStr), Bufflen));
  //
  //if Bufflen > 255 then
  //begin
  //  AStr := Space(Bufflen);
  //  AssertEquals(ErrOK, Boleto_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  //end;
  //AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_TamNossoNumero;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  //AssertEquals(ErrOK, Boleto_ConfigGravar(Handle, ''));
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Consultar Tam. Nosso Numero', ErrOK,
  //              Boleto_TamNossoNumero(Handle, '17','1212121212','123456',PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_CodigosMoraAceitos;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Consultar Cod Mora', ErrOK, Boleto_CodigosMoraAceitos(Handle, PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_SelecionaBanco;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Selecionar Banco', ErrOK, Boleto_SelecionaBanco(Handle, '001',PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_MontarNossoNumero;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  //AssertEquals(ErrOK, Boleto_ConfigGravar(Handle, ''));
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle, '..\Titulo.ini','', PChar(AStr), Bufflen));
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Montar Nosso Numero', ErrOK, Boleto_MontarNossoNumero(Handle, 0,PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_RetornaLinhaDigitavel;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  //AssertEquals(ErrOK, Boleto_ConfigGravar(Handle, ''));
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle, '..\Titulo.ini','', PChar(AStr), Bufflen));
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Retornar Linha Digitável', ErrOK, Boleto_RetornaLinhaDigitavel(Handle, 0,PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_RetornaCodigoBarras;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  //AssertEquals(ErrOK, Boleto_ConfigGravar(Handle, ''));
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos(Handle, '..\Titulo.ini','', PChar(AStr), Bufflen));
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Retornar Codigo Barras', ErrOK, Boleto_RetornaCodigoBarras(Handle, 0,PChar(AStr), Bufflen));

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;

procedure TACBrLibBoletoTest.Test_Boleto_EnviarEmail;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, Boleto_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoBoletoCedenteConfig, CChaveConvenio, '123456'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoEmail, CChaveNome, 'Jose'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoEmail, CChaveServidor, 'smtp.djsystem.com.br'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoEmail, CChaveUsuario, 'josemaria@djsystem.com.br'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoEmail, CChaveSenha, 'teste'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoEmail, CChaveEmailConta, 'josemaria@djsystem.com.br'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoEmail, CChavePorta, '587'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoEmail, CChaveEmailSSL, '0'));
  //AssertEquals(ErrOK, Boleto_ConfigGravarValor(Handle, CSessaoEmail, CChaveEmailTLS, '1'));
  //
  //AssertEquals(ErrOK, Boleto_ConfigGravar(Handle,''));
  //Bufflen := 255;
  //AStr := Space(Bufflen);
  //AssertEquals('Erro ao tentar Incluir Titulo', ErrOK, Boleto_IncluirTitulos('..\Titulo.ini','', PChar(AStr), Bufflen));
  //AssertEquals('Erro ao tentar Enviar e-mail', ErrOK, Boleto_EnviarEmail('josemaria@djsystem.com.br','Teste','Mensagem',''));

  AssertEquals(ErrOK, Boleto_Finalizar(Handle));
end;


initialization

  RegisterTest(TACBrLibBoletoTest);
end.

