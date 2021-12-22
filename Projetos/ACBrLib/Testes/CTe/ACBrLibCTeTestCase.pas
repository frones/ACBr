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

unit ACBrLibCTeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrCTeLib }

  TTestACBrCTeLib = class(TTestCase)
  published
    procedure Test_CTe_Inicializar_Com_DiretorioInvalido;
    procedure Test_CTe_Inicializar;
    procedure Test_CTe_Inicializar_Ja_Inicializado;
    procedure Test_CTe_Finalizar;
    procedure Test_CTe_Finalizar_Ja_Finalizado;
    procedure Test_CTe_ConfigLerValor;
    procedure Test_CTe_ConfigGravarValor;

    procedure Test_CTe_StatusServico;

    procedure Test_CTe_LimparLista;
    procedure Test_CTe_CarregarXML;
    procedure Test_CTe_Imprimir;
    procedure Test_CTe_ImprimirPDF;

    procedure Test_CTe_CarregarINI;
    procedure Test_CTe_Assinar;
    procedure Test_CTe_Validar;
    procedure Test_CTe_ValidarRegrasdeNegocios;
    procedure Test_CTe_VerificarAssinatura;

    procedure Test_CTe_Enviar;
    procedure Test_CTe_Consultar;
    procedure Test_CTe_Cancelar;
    procedure Test_CTe_EnviarEmail;

    procedure Test_CTe_Inutilizar;
    procedure Test_CTe_ImprimirInutilizacao;
    procedure Test_CTe_ImprimirInutilizacaoPDF;

    procedure Test_CTe_EnviarEvento;
    procedure Test_CTe_EnviarEmailEvento;
    procedure Test_CTe_ImprimirEvento;
    procedure Test_CTe_ImprimirEventoPDF;

    procedure Test_CTe_DistribuicaoDFePorUltNSU;
    procedure Test_CTe_DistribuicaoDFePorNSU;
    procedure Test_CTe_DistribuicaoDFePorChave;
  end;

implementation

uses
  ACBrLibCTeStaticImportMT, ACBrLibCTeConsts, ACBrLibConsts, Dialogs;

procedure TTestACBrCTeLib.Test_CTe_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin

  try
     //AssertEquals(ErrOk, CTE_Finalizar(Handle));
     AssertEquals(ErrDiretorioNaoExiste, CTe_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrCTeLib.Test_CTe_Inicializar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_Finalizar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, CTe_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin

  try
     AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
     AssertEquals(ErrOk, CTE_Finalizar(Handle));
     //AssertEquals(ErrOk, CTe_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrCTeLib.Test_CTe_ConfigLerValor;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  //AssertEquals(ErrOk, CTe_ConfigLerValor(Handle, CSessaoVersao, CLibCTeNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  //AssertEquals(CLibCTeVersao, AStr);
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_ConfigGravarValor;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, CTe_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CTe_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_StatusServico;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
     // Iniciando a consulta ao Status de Serviço
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao consultar ao Status de Serviço', ErrExecutandoMetodo, CTe_StatusServico(Handle, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_LimparLista;
var
  Handle: THandle;
begin
  // Iniciando a Limpeza da Lista de CT-e
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao limpar a lista de CT-e', ErrOk, CTe_LimparLista(Handle));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_CarregarXML;
var
  Handle: THandle;
begin
  // Iniciando o Carregamento do XML do CT-e
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals('Erro ao carregar o XML do CT-e', ErrOk,
  //CTe_CarregarXML(Handle, 'C:\ERP\XML\201808\CTe\35180804550110000188570010000009491283342822-cte.xml'));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_Imprimir;
var
  Handle: THandle;
begin
     // Iniciando a Impressão do DACTE
     AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
     AssertEquals('Erro ao Imprimir o DACTE', ErrExecutandoMetodo, CTe_Imprimir(Handle, '', 1, '', ''));
     AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_ImprimirPDF;
var
  Handle: THandle;
begin
     // Iniciando a geração do PDF do DACTE
     AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
     AssertEquals('Erro ao gerar o PDF do DACTE', ErrExecutandoMetodo, CTe_ImprimirPDF(Handle));
     AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_CarregarINI;
var
  Handle: THandle;
begin
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Test_CTe_LimparLista;
  //
  //// Iniciando o Carregamento do INI do CT-e
  //AssertEquals('Erro ao carregar o INI do CT-e', ErrExecutandoMetodo,
  // CTe_CarregarINI(Handle, 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\CTe\bin\Modelo-CTe.ini'));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_Assinar;
var
  Handle: THandle;
begin
  // Iniciando a Assinatura
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Assinar', ErrOk, CTe_Assinar(Handle));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_Validar;
var
  Handle: THandle;
begin
  // Iniciando a Validação
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Validar', ErrOk, CTe_Validar(Handle));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_ValidarRegrasdeNegocios;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Validação de Regras de Negócios
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Validar regras de negócio', ErrOk,
  //  CTe_ValidarRegrasdeNegocios(Handle, Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_VerificarAssinatura;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Verificação de Assinatura
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Verificar assinatura', ErrOk,
  //  CTe_VerificarAssinatura(Handle, Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_Enviar;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin

  try
     // Iniciando o Envio do Lote de CT-e
     AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

     //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

     //Resposta := '';
     //Tamanho := 0;
     //
     //AssertEquals('Erro ao Enviar Lote de CT-e', ErrOk,
     //CTe_Enviar(Handle, 1, True, Resposta, Tamanho));
     //
     //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
     AssertEquals(ErrOk, CTE_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrCTeLib.Test_CTe_Consultar;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin

  try
      AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

      //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

      //Test_CTe_LimparLista;
      //
      //// Iniciando a Consulta
      //Resposta := '';
      //Tamanho := 0;
      //
      //AssertEquals('Erro ao Consultar o CT-e', ErrOk,
      //CTe_Consultar(Handle, 'C:\ERP\XML\201808\CTe\35180804550110000188570010000009491283342822-cte.xml', Resposta, Tamanho));
      //
      //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
      //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
      //AssertEquals(ErrOk, CTE_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrCTeLib.Test_CTe_Cancelar;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Test_CTe_LimparLista;
  //
  //// Iniciando o Cancelamento
  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Cancelar o CT-e', ErrExecutandoMetodo,
  //  CTe_Cancelar(Handle, '35180804550110000188570010000009491283342822',
  //               'Desacordo comercial', '04550110000188', 1, Resposta, Tamanho));
  //
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_EnviarEmail;
begin
  //a
end;

procedure TTestACBrCTeLib.Test_CTe_Inutilizar;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Resposta := '';
  //Tamanho := 0;
  //
  //AssertEquals('Erro ao Validar', ErrExecutandoMetodo, CTE_Inutilizar(Handle, '12061411000176', 'teste de homologacao', 2019, 67, 1, 1, 1, Resposta, Tamanho));
  //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_ImprimirInutilizacao;
var
  Handle: THandle;
begin
  // Iniciando a Impressão da Inutilização
   AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

   //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals('Erro ao imprimir a Inutilização', ErrOk,
  //  CTe_ImprimirInutilizacao(Handle, 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\CTe\bin\1795714200014457017000000009000000010-ProcInutCTe.xml'));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_ImprimirInutilizacaoPDF;
var
  Handle: THandle;
begin
  // Iniciando a Geração do PDF da Inutilização
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //AssertEquals('Erro ao gerar o PDF da Inutilização', ErrExecutandoMetodo,
  //  CTe_ImprimirInutilizacaoPDF(Handle, 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\CTe\bin\1795714200014457017000000009000000010-ProcInutCTe.xml'));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_EnviarEvento;
begin
  //a
end;

procedure TTestACBrCTeLib.Test_CTe_EnviarEmailEvento;
var
  Handle:THandle;
  Path, ArqCTe, ArqEvento: String;
begin

  try
     // Iniciando o envio do evento por email
     AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

     //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

     //Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\CTe\bin\';
     //ArqCTe := Path + '28140417957142000144570170000000311556600342-cte.xml';
     //ArqEvento := Path + '2814041795714200014457017000000031155660034211011001-procEventoCTe.xml';
     //
     //AssertEquals('Erro ao enviar email do evento', ErrOk,
     //CTe_EnviarEmailEvento(Handle, 'italo.jurisato@gmail.com', PChar(ArqEvento),
     // PChar(ArqCTe), True, 'Evento', '', '',
     // 'Teste de envio de evento por email.'));
      AssertEquals(ErrOk, CTE_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrCTeLib.Test_CTe_ImprimirEvento;
var
  Handle: THandle;
  Path, ArqCTe, ArqEvento: String;
begin
  // Iniciando a Impressão do Evento
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\CTe\bin\';
  //ArqCTe := Path + '28140417957142000144570170000000311556600342-cte.xml';
  //ArqEvento := Path + '2814041795714200014457017000000031155660034211011001-procEventoCTe.xml';
  //
  //AssertEquals('Erro ao imprimir o evento', ErrOk,
  //  CTe_ImprimirEvento(Handle, PChar(ArqCTe), PChar(ArqEvento)));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_ImprimirEventoPDF;
var
  Handle: THandle;
  Path, ArqCTe, ArqEvento: String;
begin
  // Iniciando a geração do PDF do Evento
  AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

  //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

  //Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\CTe\bin\';
  //ArqCTe := Path + '28140417957142000144570170000000311556600342-cte.xml';
  //ArqEvento := Path + '2814041795714200014457017000000031155660034211011001-procEventoCTe.xml';
  //
  //AssertEquals('Erro ao gerar o PDF do evento', ErrExecutandoMetodo,
  //  CTe_ImprimirEventoPDF(Handle, PChar(ArqCTe), PChar(ArqEvento)));
  AssertEquals(ErrOk, CTE_Finalizar(Handle));
end;

procedure TTestACBrCTeLib.Test_CTe_DistribuicaoDFePorUltNSU;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin

  try
      // Iniciando a Consulta no WebServices DistribuicaoDFe
      AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

      //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

      //Resposta := '';
      //Tamanho := 0;
      //
      //AssertEquals('Erro ao Consultar o DistribuicaoDFePorUltNSU', ErrOk,
      //CTe_DistribuicaoDFePorUltNSU(Handle, 35, '04550110000188', '0', Resposta, Tamanho));
      //
      //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
      //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
      AssertEquals(ErrOk, CTE_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrCTeLib.Test_CTe_DistribuicaoDFePorNSU;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin

  try
      // Iniciando a Consulta no WebServices DistribuicaoDFe
      AssertEquals(ErrOk, CTe_Inicializar(Handle, '',''));

      //CONFIGURAÇÃO ESTA SALVO NO ACBrLib.ini

      //Resposta := '';
      //Tamanho := 0;
      //
      //AssertEquals('Erro ao Consultar o DistribuicaoDFePorNSU', ErrOk,
      //CTe_DistribuicaoDFePorNSU(Handle, 35, '04550110000188', '100', Resposta, Tamanho));
      //
      //AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
      //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
      AssertEquals(ErrOk, CTE_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrCTeLib.Test_CTe_DistribuicaoDFePorChave;
begin
  // A SEFAZ ainda não disponibilizou a consulta pela chave no webservice
  // DistribuicaoDFe
end;

initialization
  RegisterTest(TTestACBrCTeLib);

end.

