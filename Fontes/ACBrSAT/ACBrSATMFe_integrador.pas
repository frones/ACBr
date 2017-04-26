{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  André Ferreira Moraes                          }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrSATMFe_integrador ;

interface

uses
  Classes, SysUtils, ACBrSATClass, pcnGerador, pcnMFeUtil, pcnLeitor, pcnEnviarPagamento;

type

   { TACBrSATMFe_integrador_XML }

   TACBrSATMFe_integrador_XML = class( TACBrSATClass )
   private
     FGerador: TGerador;
     FIdentificador: TIdentificador;
     FParametro: TParametro;
     FMetodo: TMetodo;

     FLeitor : TLeitor;
     FPastaInput : String;
     FPastaOutput : String;
     FTimeout : Integer;

     function EnviaComando(Comando : String) : String;
     function PegaResposta(Resp : String) : String;

     function AguardaArqResposta : String;

     function AjustaComando(Comando : String) : String;

   protected
     procedure LoadDLLFunctions ; override;
     procedure UnLoadDLLFunctions; override;

   public
     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy; override;

     function AssociarAssinatura( CNPJvalue, assinaturaCNPJs : AnsiString ):
       String ; override;
     function AtivarSAT( subComando : Integer; CNPJ: AnsiString; cUF : Integer )
       : String ; override;
     function AtualizarSoftwareSAT : String ; override;
     function BloquearSAT : String ; override;
     function CancelarUltimaVenda( chave, dadosCancelamento : AnsiString ) :
       String ; override;
     function ComunicarCertificadoICPBRASIL( certificado : AnsiString ) :
       String ; override;
     function ConfigurarInterfaceDeRede( dadosConfiguracao : AnsiString ) :
       String ; override;
     function ConsultarNumeroSessao( cNumeroDeSessao : Integer) : String ;
       override;
     function ConsultarSAT : String ; override ;
     function ConsultarStatusOperacional : String ; override;
     function DesbloquearSAT : String ; override;
     function EnviarDadosVenda( dadosVenda : AnsiString ) : String ; override;
     function ExtrairLogs : String ; override;
     function TesteFimAFim( dadosVenda : AnsiString) : String ; override;
     function TrocarCodigoDeAtivacao( codigoDeAtivacaoOuEmergencia: AnsiString;
       opcao : Integer; novoCodigo: AnsiString ) : String ; override;

     function EnviarPagamento( Pagamento : TEnviarPagamento ): String;

   published
     property PastaInput  : String  read FPastaInput  write FPastaInput;
     property PastaOutput : String  read FPastaOutput write FPastaOutput;
     property Timeout     : Integer read FTimeout     write FTimeout default 30;
   end;

implementation

Uses ACBrUtil, pcnConversao, dateutils, ACBrSAT;

function TACBrSATMFe_integrador_XML.EnviaComando(Comando: String): String;
var
  SL : TStringList;
  TimeOut, ActualTime : TDateTime;
begin
  SL := TStringList.Create;
  try
    SL.Add(Comando);
    SL.SaveToFile(PathWithDelim(FPastaInput)+'Comando.xml');

    ActualTime := Now;
    if FTimeout <= 0 then
      TimeOut := IncSecond(ActualTime, 30)
    else
      TimeOut := IncSecond(ActualTime, FTimeout);
    Result := AguardaArqResposta;
    while EstaVazio(Result) and
          (ActualTime < TimeOut) do
    begin
      Result := AguardaArqResposta;
      Sleep(100);
      ActualTime := Now;
    end;
  finally
    SL.Free;
  end;
end;

function TACBrSATMFe_integrador_XML.PegaResposta(Resp: String): String;
begin
  FLeitor.Arquivo := Resp;
  if FLeitor.rExtrai(1, 'Resposta') <> '' then
    Result := FLeitor.rCampo(tcStr, 'retorno')
  else if FLeitor.rExtrai(1, 'Erro') <> '' then
    Result := FLeitor.Grupo;
end;

function TACBrSATMFe_integrador_XML.AguardaArqResposta: String;
var
  SL, SLArqResp : TStringList;
  I : Integer;
begin
  SL := TStringList.Create;
  SLArqResp := TStringList.Create;
  try
    SLArqResp.Clear;
    FindFiles(PathWithDelim(FPastaOutput)+'*.xml',SLArqResp);
    Sleep(100); //Tentar evitar ler arquivo enquanto está sendo escrito

    for I:=0  to SLArqResp.Count-1 do
    begin
      SL.Clear;
      SL.LoadFromFile(SLArqResp[I]);
      FLeitor.Arquivo := SL.Text;
      if FLeitor.rExtrai(1, 'Identificador') <> '' then
      begin
        if FLeitor.rCampo(tcInt, 'Valor') = numeroSessao then
        begin
          Result := Trim(FLeitor.Arquivo);
          DeleteFile(SLArqResp[I]);
        end;
      end;
    end;
  finally
    SLArqResp.Free;
    SL.Free;
  end;
end;

function TACBrSATMFe_integrador_XML.AjustaComando(Comando: String): String;
begin
  Comando := ChangeLineBreak(Comando,'');

  while pos('  ', Comando) > 0 do
    Comando := StringReplace(Comando, '  ', ' ', [rfReplaceAll]);

  Comando := StringReplace(Comando, '> <', '><', [rfReplaceAll]);;
  Result := Comando;
end;

procedure TACBrSATMFe_integrador_XML.LoadDLLFunctions;
begin
  //Não faz nada
end;

procedure TACBrSATMFe_integrador_XML.UnLoadDLLFunctions;
begin
  //Não faz nada
end;

constructor TACBrSATMFe_integrador_XML.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fpModeloStr := 'MFe_Integrador_XML' ;
  FGerador       := TGerador.Create;
  FIdentificador := TIdentificador.Create(FGerador);
  FParametro     := TParametro.Create(FGerador);
  FMetodo        := TMetodo.Create(FGerador);
  FLeitor        := TLeitor.Create;

  FPastaInput  := 'C:\Integrador\Input\';
  FPastaOutput := 'C:\Integrador\Output\';
end ;

destructor TACBrSATMFe_integrador_XML.Destroy;
begin
  FIdentificador.Free;
  FParametro.Free;
  FMetodo.Free;
  FGerador.Free;
  FLeitor.Free;
  inherited Destroy;
end;

function TACBrSATMFe_integrador_XML.AssociarAssinatura(CNPJvalue,
  assinaturaCNPJs : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','AssociarAssinatura');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('cnpjValue',CNPJvalue,tcStr);
  FParametro.GerarParametro('assinaturaCNPJs',assinaturaCNPJs,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.AtivarSAT(subComando : Integer ;
  CNPJ : AnsiString; cUF : Integer) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','AtivarMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('subComando',subComando,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('CNPJ',CNPJ,tcStr);
  FParametro.GerarParametro('cUF',cUF,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.AtualizarSoftwareSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','AtualizarSoftwareMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigodeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.BloquearSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','BloquearMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigodeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.CancelarUltimaVenda(chave,
  dadosCancelamento : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','CancelarUltimaVenda');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigodeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('chave',chave,tcStr);
  FParametro.GerarParametro('dadosCancelamento','<![CDATA[' +AjustaComando(dadosCancelamento)+ ']]>',tcStr, False);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ComunicarCertificadoICPBRASIL(
  certificado : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ComunicarCertificadoICPBRASIL');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('certificado',certificado,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);
  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConfigurarInterfaceDeRede(
  dadosConfiguracao : AnsiString) : String ;
Var
  Resp : String;
begin
{  Resp := xSAT_ConfigurarInterfaceDeRede( numeroSessao,
                 String(codigoDeAtivacao), String(dadosConfiguracao) ) ; }

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConsultarNumeroSessao(cNumeroDeSessao : Integer
  ) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ConsultarNumeroSessao');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('cNumeroDeSessao',cNumeroDeSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConsultarSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ConsultarMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConsultarStatusOperacional : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ConsultarStatusOperacional');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigodeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.DesbloquearSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','DesbloquearMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigodeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.EnviarDadosVenda(dadosVenda : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','EnviarDadosVenda');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigodeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('dadosVenda','<![CDATA[' +AjustaComando(dadosVenda)+ ']]>',tcStr, False);
  FParametro.GerarParametro('nrDocumento',numeroSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);
  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ExtrairLogs : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ExtrairLogs');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigodeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);
  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.TesteFimAFim(dadosVenda : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','TesteFimaFim');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigodeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('dadosVenda','<![CDATA[' +AjustaComando(dadosVenda)+ ']]>',tcStr, False);
//  FParametro.GerarParametro('nrDocumento',numeroSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.TrocarCodigoDeAtivacao(
  codigoDeAtivacaoOuEmergencia: AnsiString; opcao: Integer; novoCodigo: AnsiString
  ): String;
Var
  Resp : String;
begin
  if codigoDeAtivacaoOuEmergencia = '' then
    codigoDeAtivacaoOuEmergencia := codigoDeAtivacao;

  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','TrocarCodigoDeAtivacao');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigodeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('opcao',opcao,tcInt);
  FParametro.GerarParametro('novoCodigo',novoCodigo,tcStr);
  FParametro.GerarParametro('confNovoCodigo',novoCodigo,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := EnviaComando(FGerador.ArquivoFormatoXML);

  Result := PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.EnviarPagamento(Pagamento: TEnviarPagamento
  ): String;
var
  Comando, Resp : String;
begin
  TACBrSAT(Owner).IniciaComando;

  Pagamento.Identificador := numeroSessao;
  Comando := Pagamento.GetXMLString;
  TACBrSAT(Owner).fsComandoLog := 'EnviarPagamento( '+Comando+' )';


  Resp := EnviaComando(Comando);
  Result := PegaResposta( Resp );
  TACBrSAT(Owner).FinalizaComando( Result );
end;

end.

