{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}
unit DoACBrNFeXMLUnit;

interface

uses CmdUnit, ACBrNFeWebServices, pcnConversao, ACBrUtil.Base,
     Classes, TypInfo, SysUtils, Types;

procedure DoACBrNFeXML( Cmd : TACBrCmd ) ;
function LoadURL(WebService : TWebServices; tpEvento: TpcnTpEvento = teCancelamento) : String;
function LoadSoapAction(WebService : TWebServices) : String;
function TipoWebService( XML : String ) : TWebServices;
function AssinarXML( WebService : TWebServices; XML : String ) : String;
function TipoEvento( XML : String ) : TpcnTpEvento;

implementation

Uses  ACBrMonitor1, pcnAuxiliar, pcnLeitor;

procedure DoACBrNFeXML( Cmd : TACBrCmd ) ;
var
  TPWebService : TWebServices;
begin
 with FrmACBrMonitor do
  begin
     try
        TPWebService := TipoWebService( Cmd.Comando );

        if TPWebService is TNFeEnvEvento then
           ACBrNFe1.WebServices.EnvioWebService.URLEnvio        := LoadURL( TPWebService, TipoEvento( Cmd.Comando ) )
        else
           ACBrNFe1.WebServices.EnvioWebService.URLEnvio        := LoadURL( TPWebService );

        ACBrNFe1.WebServices.EnvioWebService.SoapActionEnvio := LoadSoapAction( TPWebService );
        ACBrNFe1.WebServices.EnvioWebService.XMLEnvio        := AssinarXML( TPWebService, Cmd.Comando );
        ACBrNFe1.WebServices.EnvioWebService.Executar;
        Cmd.Resposta := ACBrNFe1.WebServices.EnvioWebService.RetWS;
     finally
        { Nada a fazer aqui por enquanto... :) }
     end ;
  end;
end;

function LoadURL(WebService : TWebServicesBase; tpEvento: TpcnTpEvento = teCancelamento) : String;
begin
  if WebService is TNFeStatusServico then
    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNfeStatusServico)
  else if WebService is TNFeRecepcao then
    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNfeRecepcao)
  else if (WebService is TNFeRetRecepcao) or (WebService is TNFeRecibo) then
    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNfeRetRecepcao)
  else if WebService is TNFeConsulta then
    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNfeConsulta)
//  else if WebService is TNFeCancelamento then
//    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNfeCancelamento)
  else if WebService is TNFeInutilizacao then
    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNfeInutilizacao)
  else if WebService is TNFeConsultaCadastro then
    Result  := NotaUtil.GetURL(UFparaCodigo(TNFeConsultaCadastro(WebService).UF), FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNfeCadastro)
  else if WebService is TNFeEnvDPEC then
    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNfeEnvDPEC)
  else if WebService is TNFeConsultaDPEC then
    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNfeConsultaDPEC)
//  else if WebService is TNFeCartaCorrecao then
//    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNFeCCe)
  else if WebService is TNFeEnvEvento then
  begin
    //Verificação necessária pois somente os eventos de Cancelamento e CCe serão tratados pela SEFAZ do estado
    //os outros eventos como manifestacao de destinatários serão tratados diretamente pela RFB
    if not (tpEvento in [teCCe,teCancelamento]) then
      Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNFeEventoAN)
    else
      Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNFeEvento)
  end
  else if WebService is TNFeConsNFeDest then
    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNFeConsNFeDest)
  else if WebService is TNFeDownloadNFe then
    Result  := NotaUtil.GetURL(FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.UFCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo, LayNFeDownloadNFe);
end;

function LoadSoapAction(WebService : TWebServicesBase) : String;
begin
  if WebService is TNFeStatusServico then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/NfeStatusServico2'
  else if WebService is TNFeRecepcao then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/NfeRecepcao2'
  else if (WebService is TNFeRetRecepcao) or (WebService is TNFeRecibo) then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/NfeRetRecepcao2'
  else if WebService is TNFeConsulta then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/NfeConsulta2'
//  else if WebService is TNFeCancelamento then
//    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/NfeCancelamento2'
  else if WebService is TNFeInutilizacao then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/NfeInutilizacao2'
  else if WebService is TNFeConsultaCadastro then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/CadConsultaCadastro2'
  else if WebService is TNFeEnvDPEC then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/SCERecepcaoRFB/sceRecepcaoDPEC'
  else if WebService is TNFeConsultaDPEC then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/SCEConsultaRFB/sceConsultaDPEC'
  else if WebService is TNFeEnvEvento then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/RecepcaoEvento'
  else if WebService is TNFeConsNFeDest then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/NfeConsultaDest/nfeConsultaNFDest'
  else if WebService is TNFeDownloadNFe then
    Result  := 'http://www.portalfiscal.inf.br/nfe/wsdl/NfeDownloadNF/nfeDownloadNF'
end;

function TipoWebService( XML : String ) : TWebServicesBase;
begin
  if pos('<consStatServ',XML) > 0 then
     Result := FrmACBrMonitor.ACBrNFe1.WebServices.StatusServico
  else if pos('<enviNFe',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.Enviar
  else if pos('<consReciNFe',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.Retorno
  else if pos('<consSitNFe',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.Consulta
//  else if pos('<cancNFe',XML) > 0 then
//    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.Cancelamento
  else if pos('<inutNFe',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.Inutilizacao
  else if pos('<ConsCad',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.ConsultaCadastro
  else if pos('<envDPEC',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.EnviarDPEC
  else if pos('<consDPEC',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.ConsultaDPEC
  else if pos('<envEvento',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.EnvEvento
  else if pos('<consNFeDest',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.ConsNFeDest
  else if pos('<downloadNFe',XML) > 0 then
    Result  := FrmACBrMonitor.ACBrNFe1.WebServices.DownloadNFe
  else
     raise Exception.Create('Arquivo não reconhecido');  
end;

function AssinarXML( WebService : TWebServicesBase; XML : String ) : String;
var
  DadosMsg, Msg : String;
  i, f : integer;
  Eventos, Evento, Lote, EventosAssinados: AnsiString;
  Notas, NFe, NFesAssinadas: AnsiString;
begin
 if {(WebService is TNFeCancelamento) or}
    (WebService is TNFeEnvEvento) or
    (WebService is TNFeInutilizacao) or
    (WebService is TNFeEnvDPEC) or
    (WebService is TNFeRecepcao) then
  begin
    if WebService is TNFeEnvEvento then
     begin
       i       := Pos( '<evento ', XML );
       Lote    := Copy( XML, 1, i - 1 );
       Eventos := SeparaDados( XML, 'envEvento' );
       i       := Pos( '<evento ', Eventos );
       Eventos := Copy( Eventos, i, length(Eventos) );

       EventosAssinados := '';

       // Realiza a assinatura para cada evento
       while Eventos <> '' do
        begin
         f := Pos( '</evento>', Eventos );

         if f > 0 then
          begin
           Evento  := Copy( Eventos, 1, f + 8 );
           Eventos := Copy( Eventos, f + 9, length(Eventos) );

       {$IFDEF ACBrNFeOpenSSL}
           if not(NotaUtil.Assinar(Evento, FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.Certificado , FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.Senha, DadosMsg, Msg)) then
              begin
                raise Exception.Create('Falha ao assinar o Envio de Evento '+LineBreak+Msg);
              end;
       {$ELSE}
           if not(NotaUtil.Assinar(Evento, FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.GetCertificado , DadosMsg, Msg)) then
              begin
                raise Exception.Create('Falha ao assinar o Envio de Evento '+LineBreak+Msg);
              end;
       {$ENDIF}

           if not(NotaUtil.Valida(DadosMsg, Msg, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.PathSchemas)) then
            begin
              raise Exception.Create('Falha na validação do Evento '+LineBreak+Msg);
            end;

           EventosAssinados := EventosAssinados + DadosMsg;
          end
         else
            Eventos := '';
        end;

       f := Pos( '?>', EventosAssinados );
       if f <> 0 then
         DadosMsg := copy(EventosAssinados,1,f+1) +
                     Lote +
                     copy(EventosAssinados,f+2,Length(EventosAssinados)) +
                     '</envEvento>'
       else
         DadosMsg := Lote + EventosAssinados + '</envEvento>';
     end
    else if WebService is TNFeRecepcao then
     begin
       Lote    := '';
       i       := Pos( '<NFe ', XML );
       Lote    := Copy( XML, 1, i - 1 );
       Notas := SeparaDados( XML, 'enviNFe' );
       i       := Pos( '<NFe ', Notas );
       Notas := Copy( Notas, i, length(Notas) );

       NFesAssinadas := '';

       // Realiza a assinatura para cada evento
       while Notas <> '' do
        begin
         f := Pos( '</NFe>', Notas );

         if f > 0 then
          begin
           NFe  := Copy( Notas, 1, f + 8 );
           Notas := Copy( Notas, f + 9, length(Notas) );

       {$IFDEF ACBrNFeOpenSSL}
           if not(NotaUtil.Assinar(NFe, FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.Certificado , FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.Senha, DadosMsg, Msg)) then
              begin
                raise Exception.Create('Falha ao assinar a NFe '+LineBreak+Msg);
              end;
       {$ELSE}
           if not(NotaUtil.Assinar(NFe, FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.GetCertificado , DadosMsg, Msg)) then
              begin
                raise Exception.Create('Falha ao assinar a NFe '+LineBreak+Msg);
              end;
       {$ENDIF}

           if not(NotaUtil.Valida(DadosMsg, Msg, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.PathSchemas)) then
            begin
              raise Exception.Create('Falha na validação da NFe '+LineBreak+Msg);
            end;

           NFesAssinadas := NFesAssinadas + DadosMsg;
          end
         else
           Notas := '';
        end;

       f := Pos( '?>', NFesAssinadas );
       if f <> 0 then
         DadosMsg := copy(NFesAssinadas,1,f+1) +
                     Lote +
                     copy(NFesAssinadas,f+2,Length(NFesAssinadas)) +
                     '</enviNFe>'
       else
         DadosMsg := Lote + NFesAssinadas + '</enviNFe>';
     end
    else
     begin
{$IFDEF ACBrNFeOpenSSL}
        if not(NotaUtil.Assinar(XML, FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.Certificado , FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.Senha, DadosMsg, Msg)) then
           raise Exception.Create('Falha ao assinar o XML '+LineBreak+Msg);
{$ELSE}
        if not(NotaUtil.Assinar(XML, FrmACBrMonitor.ACBrNFe1.Configuracoes.Certificados.GetCertificado , DadosMsg, Msg)) then
           raise Exception.Create('Falha ao assinar o XML '+LineBreak+Msg);
{$ENDIF}
       if not(NotaUtil.Valida(DadosMsg, Msg, FrmACBrMonitor.ACBrNFe1.Configuracoes.Geral.PathSchemas)) then
        begin
          raise Exception.Create('Falha na validação do XML '+LineBreak+Msg);
        end;
     end;
    Result := DadosMsg;
  end
 else
    Result := XML;
end;

function TipoEvento( XML : String ) : TpcnTpEvento;
var
  ok : boolean;
  LeitorXML : TLeitor;
begin
  LeitorXML := TLeitor.Create;
  try
     LeitorXML.Arquivo := XML;
     LeitorXML.Grupo := XML;
     Result := StrToTpEvento(ok,LeitorXML.rCampo(tcStr, 'tpEvento'));
  finally
     LeitorXML.Free;
  end;
end;

end.
