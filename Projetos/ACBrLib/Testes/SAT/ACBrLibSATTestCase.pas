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

unit ACBrLibSATTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibSATNome = 'ACBrLibSAT';

type

  { TTestACBrSATLib }

  TTestACBrSATLib = class(TTestCase)
  published
    procedure Test_SAT_Inicializar_Com_DiretorioInvalido;
    procedure Test_SAT_Inicializar;
    procedure Test_SAT_Inicializar_Ja_Inicializado;
    procedure Test_SAT_Finalizar;
    procedure Test_SAT_Finalizar_Ja_Finalizado;
    procedure Test_SAT_Nome_Obtendo_LenBuffer;
    procedure Test_SAT_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_SAT_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_SAT_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_SAT_Versao;
    procedure Test_SAT_ConfigLerValor;
    procedure Test_SAT_ConfigGravarValor;
    procedure Test_SAT_CriarCFe;
    procedure Test_SAT_EnviarCFe;
    procedure Test_SAT_ValidarCFe;
    procedure Test_SAT_CriarEnviarCFe;
    procedure Test_SAT_ImpressaoExtratoFortes;
    procedure Test_SAT_ImpressaoExtratoEscPOS;
    procedure Test_SAT_ImpressaoExtratoPDF_Sem_NomeArquivo;
    procedure Test_SAT_ImpressaoExtratoPDF_Com_NomeArquivo;
    procedure Test_SAT_ImpressaoExtratoPDF_Com_PathPDF;
    procedure Test_SAT_SetNumeroSessao;
    procedure Test_SAT_SalvarPDF;
  end;

implementation

uses
  Printers, OSPrinters,
  ACBrLibSATStaticImportMT, ACBrLibSATConsts, ACBrLibConsts, ACBrUtil, Dialogs;

procedure TTestACBrSATLib.Test_SAT_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  AssertEquals(ErrDiretorioNaoExiste, SAT_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrSATLib.Test_SAT_Inicializar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, SAT_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_Finalizar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
    Handle := 0;
    AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
    AssertEquals(ErrOk, SAT_Finalizar(Handle));
    {
     try
       SAT_Finalizar(Handle);
     except
       Exit;
     end;
     Fail('Não ocorreu erro ao finalizar uma segunda vez');
    }
end;

procedure TTestACBrSATLib.Test_SAT_Nome_Obtendo_LenBuffer;
var
  Handle: THandle;
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, SAT_Nome(Handle, Nil, Bufflen));
  AssertEquals(Length(CLibSATNome), Bufflen);
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_Nome_Lendo_Buffer_Tamanho_Identico;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  Bufflen := Length(CLibSATNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_Nome(Handle,PChar(AStr), Bufflen));
  AssertEquals(Length(CLibSATNome), Bufflen);
  AssertEquals(CLibSATNome, AStr);
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_Nome_Lendo_Buffer_Tamanho_Maior;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  Bufflen := Length(CLibSATNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_Nome(Handle,PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibSATNome), Bufflen);
  AssertEquals(CLibSATNome, AStr);
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_Nome_Lendo_Buffer_Tamanho_Menor;
var
  Handle: THandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_Nome(Handle,PChar(AStr), Bufflen));
  AssertEquals(Length(CLibSATNome), Bufflen);
  AssertEquals(copy(CLibSATNome,1,4), AStr);
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_Versao;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, SAT_Versao(Handle,Nil, Bufflen));
  Assert(Bufflen > 0);

  // Lendo a resSATta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_Versao(Handle,PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert(AStr <> '');
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_ConfigLerValor;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_ConfigLerValor(Handle,CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_ConfigGravarValor;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_CriarCFe;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveModelo, '1'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveCodigoDeAtivacao, 'sefaz1234'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveSignAC, '111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111'));
  //AssertEquals(ErrOK, SAT_ConfigGravar(Handle, ''));

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar criar o CFe', ErrOK, SAT_CriarCFe(Handle, '..\CFe.ini', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_EnviarCFe;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveCodigoDeAtivacao, 'sefaz1234'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveSignAC, '111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111'));
  //AssertEquals(ErrOK, SAT_ConfigGravar(Handle, ''));

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar enviar o CFe', ErrExecutandoMetodo, SAT_EnviarCFe(Handle, '..\001-000000-satcfe.xml', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_ValidarCFe;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));

  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveCodigoDeAtivacao, 'sefaz1234'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveSignAC, '111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSATConfig, CChaveArqSchema, '..\ACBr\Exemplos\ACBrSAT\Schemas\Schemas\CfeDadosVendaAPL_0008.xsd'));
  //AssertEquals(ErrOk, SAT_ConfigGravarValor(Handle, CSessaoSATConfig, CChaveSSLXmlSignLib, '4'));
  //AssertEquals(ErrOk, SAT_ConfigGravarValor(Handle, CSessaoDFe, CChaveSSLCryptLib, '1'));

  AssertEquals(ErrOK, SAT_ConfigGravar(Handle, ''));

  AssertEquals('Erro ao tentar validar o CFe', ErrOK, SAT_ValidarCFe(Handle, '..\001-000000-satcfe.xml'));

  AssertEquals(ErrOK, SAT_Finalizar(Handle));

end;

procedure TTestACBrSATLib.Test_SAT_CriarEnviarCFe;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveModelo, '1'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveCodigoDeAtivacao, 'sefaz1234'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveSignAC, '111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111'));
  //AssertEquals(ErrOK, SAT_ConfigGravar(Handle, ''));

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar criar e enviar o CFe', ErrExecutandoMetodo, SAT_CriarEnviarCFe(Handle, '..\CFe.ini', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_ImpressaoExtratoFortes;
var
  Handle: THandle;
  NomeImpressoraPDF: String;
  I: Integer;
begin
  NomeImpressoraPDF := '';
  I := 0;
  while (I < Printer.Printers.Count) and (NomeImpressoraPDF = '') do
  begin
    if (pos(' PDF', UpperCase(Printer.Printers[I])) > 0) then
      NomeImpressoraPDF := Printer.Printers[I];

    Inc( I );
  end;

  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveModelo, '1'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChaveTipo, '0'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChavePrinterName, PChar(NomeImpressoraPDF)));
  //AssertEquals(ErrOK, SAT_ConfigGravar(Handle, ''));

  AssertEquals(ErrOK, SAT_ImprimirExtratoVenda(Handle, '..\AD35180911111111111111591234567890001684429520.xml', ''));

  AssertEquals(ErrOK, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_ImpressaoExtratoEscPOS;
var
  Handle: THandle;
  //SaidaImpressao: String;
begin
  //SaidaImpressao := ApplicationPath+'posprinter.txt';

  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));

  ////CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveModelo, '1'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChaveTipo, '1'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPosPrinter, CChaveModelo, '1'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPosPrinter, CChavePorta, PChar(SaidaImpressao)));
  //AssertEquals(ErrOK, SAT_ConfigGravar(Handle, ''));

  AssertEquals(ErrOK, SAT_ImprimirExtratoVenda(Handle, '..\AD35180911111111111111591234567890001684429520.xml', ''));

  AssertEquals(ErrOK, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_ImpressaoExtratoPDF_Sem_NomeArquivo;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr, PDFFile: String;
begin
  PDFFile := ApplicationPath+'pdf'+PathDelim+'CFe-3518_0911_1111_1111_1111_5912_3456_7890_0016_8442_9520.pdf';
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveModelo, '1'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChaveTipo, '0'));
  //AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChavePathPDF, ''));
  //AssertEquals(ErrOK, SAT_ConfigGravar(Handle, ''));

  if FileExists(PDFFile) then
    DeleteFile(PDFFile);

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar geara PDF do CFe', ErrOK,
                SAT_GerarPDFExtratoVenda(Handle, '..\AD35180911111111111111591234567890001684429520.xml',
                                         '', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar(Handle));
  AssertTrue(FileExists(PDFFile));
end;

procedure TTestACBrSATLib.Test_SAT_ImpressaoExtratoPDF_Com_NomeArquivo;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr, PDFFile: String;
begin
  PDFFile := ApplicationPath+'pdf'+PathDelim+'AD35180911111111111111591234567890001684429520.pdf';
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChaveTipo, '0'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChavePathPDF, ''));
  AssertEquals(ErrOK, SAT_ConfigGravar(Handle, ''));

  if FileExists(PDFFile) then
    DeleteFile(PDFFile);

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar geara PDF do CFe', ErrOK,
                SAT_GerarPDFExtratoVenda(Handle, '..\AD35180911111111111111591234567890001684429520.xml',
                                         PChar(PDFFile), PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar(Handle));
  AssertTrue(FileExists(PDFFile));
end;

procedure TTestACBrSATLib.Test_SAT_ImpressaoExtratoPDF_Com_PathPDF;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr, PDFFile: String;
begin
  PDFFile := ApplicationPath+'..'+PathDelim+'AD35180911111111111111591234567890001684429520.pdf';
  AssertEquals(ErrOk, SAT_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChaveTipo, '0'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChavePathPDF, PChar(ApplicationPath+'..'+PathDelim)));
  AssertEquals(ErrOK, SAT_ConfigGravar(Handle, ''));

  if FileExists(PDFFile) then
    DeleteFile(PDFFile);

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar geara PDF do CFe', ErrOK,
                SAT_GerarPDFExtratoVenda(Handle, '..\AD35180911111111111111591234567890001684429520.xml',
                                         'AD35180911111111111111591234567890001684429520.pdf',
                                         PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(Handle, PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_ConfigGravarValor(Handle, CSessaoExtrato, CChavePathPDF, ''));
  AssertEquals(ErrOK, SAT_Finalizar(Handle));
  AssertTrue(FileExists(PDFFile));
end;

procedure TTestACBrSATLib.Test_SAT_SetNumeroSessao;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle,'',''));
  AssertEquals(ErrOK, SAT_SetNumeroSessao(Handle,'92238'));
  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

procedure TTestACBrSATLib.Test_SAT_SalvarPDF;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOk, SAT_Inicializar(Handle,'',''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro ao Salvar PDF', ErrOK,
  SAT_SalvarPDF(Handle, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOk, SAT_Finalizar(Handle));
end;

initialization
  RegisterTest(TTestACBrSATLib);

end.

