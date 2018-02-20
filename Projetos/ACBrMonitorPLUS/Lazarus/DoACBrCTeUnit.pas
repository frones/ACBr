{******************************************************************************}
{ Projeto: ACBrNFeMonitor                                                      }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}
{$I ACBr.inc}

unit DoACBrCTeUnit;

interface
Uses Classes, SysUtils, CmdUnit, Dialogs;

Procedure DoACBrCTe( Cmd : TACBrCmd );
procedure GerarIniCTe( AStr: String );
function GerarCTeIni( XML : String ) : String;
procedure GerarCTeIniEvento( AStr: String );

implementation

Uses IniFiles, DateUtils, strutils,
  Forms, ACBrUtil, ACBrMonitor1,
  pcnConversao, pcteConversaoCTe,
  pcteCTeR, pcnAuxiliar, DoACBrUnit, DoACBrNFeUnit, ACBrDFeUtil;

Procedure DoACBrCTe( Cmd : TACBrCmd );
var
  I,J : Integer;
  ArqCTe, ArqPDF, ArqEvento, Chave : String;
  Salva,  OK : Boolean;
  SL     : TStringList;
  Alertas : AnsiString;

  sMensagemEmail: TStringList;
  CC, Anexos: Tstrings;
  Memo   , PathsCTe: TStringList;
  Files  : String;
  dtFim  : TDateTime;
  sTemMais : String;

  RetFind   : Integer;
  SearchRec : TSearchRec;
  bMostrarPreview : Boolean;
  tipoEvento: TpcnTpEvento;
  FormaEmissao: TpcnTipoEmissao;

  VersaoDFCTe  : TVersaoCTe;
  ModeloDFCTe  : TModeloCTe;

begin
 with FrmACBrMonitor do
  begin
     try
        if Cmd.Metodo = 'statusservico' then
         begin
           if ACBrCTe1.WebServices.StatusServico.Executar then
            begin
              Cmd.Resposta := ACBrCTe1.WebServices.StatusServico.Msg+
                              '[STATUS]'+sLineBreak+
                              'Versao='+ACBrCTe1.WebServices.StatusServico.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.StatusServico.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrCTe1.WebServices.StatusServico.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrCTe1.WebServices.StatusServico.CStat)+sLineBreak+
                              'XMotivo='+ACBrCTe1.WebServices.StatusServico.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrCTe1.WebServices.StatusServico.CUF)+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrCTe1.WebServices.StatusServico.DhRecbto)+sLineBreak+
                              'TMed='+IntToStr(ACBrCTe1.WebServices.StatusServico.TMed)+sLineBreak+
                              'DhRetorno='+DateTimeToStr(ACBrCTe1.WebServices.StatusServico.DhRetorno)+sLineBreak+
                              'XObs='+ACBrCTe1.WebServices.StatusServico.XObs+sLineBreak;
            end;
         end
         
        else if Cmd.Metodo = 'validarcte' then
         begin
           ACBrCTe1.Conhecimentos.Clear;
           CarregarDFe(Cmd.Params(0), ArqCTe,tDFeCTe);
           ACBrCTe1.Conhecimentos.Validar;
         end

        else if Cmd.Metodo = 'assinarcte' then
         begin
           ACBrCTe1.Conhecimentos.Clear;
           CarregarDFe(Cmd.Params(0), ArqCTe, tDFeCTe);
           Salva := ACBrCTe1.Configuracoes.Geral.Salvar;

           if not Salva then
           begin
             ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
             ACBrCTe1.Configuracoes.Arquivos.PathSalvar := PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs';
           end;

           ACBrCTe1.Configuracoes.Geral.Salvar := True;
           ACBrCTe1.Conhecimentos.Assinar;
           ACBrCTe1.Configuracoes.Geral.Salvar := Salva;

           if NaoEstaVazio(ACBrCTe1.Conhecimentos.Items[0].NomeArq) then
              Cmd.Resposta := ACBrCTe1.Conhecimentos.Items[0].NomeArq
           else
              Cmd.Resposta := PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+StringReplace(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.ID, 'CTe', '', [rfIgnoreCase])+'-cte.xml';
         end

        else if Cmd.Metodo = 'consultarcte' then
         begin
           ACBrCTe1.Conhecimentos.Clear;

           PathsCTe := TStringList.Create;
           try
             PathsCTe.Append(Cmd.Params(0));
             PathsCTe.Append(PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             try
               CarregarDFe(PathsCTe, ArqCTe, tDFeCTe);
             except
             end;
           finally
             PathsCTe.Free;
           end;

           if ACBrCTe1.Conhecimentos.Count = 0 then
           begin
             if ValidarChave(Cmd.Params(0)) then
                ACBrCTe1.WebServices.Consulta.CTeChave := Cmd.Params(0)
             else
               raise Exception.create('Parâmetro inválido. Chave inválida ou arquivo não encontrado.');
           end
           else
             ACBrCTe1.WebServices.Consulta.CTeChave := OnlyNumber(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.ID);

           try
             ACBrCTe1.WebServices.Consulta.Executar;
             Cmd.Resposta := ACBrCTe1.WebServices.Consulta.Msg+sLineBreak+
                              '[CONSULTA]'+sLineBreak+
                              'Versao='+ACBrCTe1.WebServices.Consulta.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Consulta.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrCTe1.WebServices.Consulta.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrCTe1.WebServices.Consulta.CStat)+sLineBreak+
                              'XMotivo='+ACBrCTe1.WebServices.Consulta.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrCTe1.WebServices.Consulta.CUF)+sLineBreak+
                              'ChCTe='+ACBrCTe1.WebServices.Consulta.CTeChave+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrCTe1.WebServices.Consulta.DhRecbto)+sLineBreak+
                              'NProt='+ACBrCTe1.WebServices.Consulta.Protocolo+sLineBreak+
                              'DigVal='+ACBrCTe1.WebServices.Consulta.protCTe.digVal+sLineBreak+
                              'XML='+StringReplace(ParseText(FiltrarTextoXML(True,ChangeLineBreak(ACBrCTe1.WebServices.Consulta.RetWS,''))),'> <', '><', [rfReplaceAll])+sLineBreak;

           except
              raise Exception.Create(ACBrCTe1.WebServices.Consulta.Msg);
           end;
         end

        else if Cmd.Metodo = 'cancelarcte' then
         begin
           if not ValidarChave(Cmd.Params(0)) then
             raise Exception.Create('Chave '+Cmd.Params(0)+' inválida.')
           else
             ACBrCTe1.WebServices.Consulta.CTeChave := Cmd.Params(0);

           if not ACBrCTe1.WebServices.Consulta.Executar then
             raise Exception.Create(ACBrCTe1.WebServices.Consulta.Msg);

           ACBrCTe1.EventoCTe.Evento.Clear;
           with ACBrCTe1.EventoCTe.Evento.Add do
           begin
             infEvento.CNPJ := Cmd.Params(2);
             if Trim(infEvento.CNPJ) = '' then
                infEvento.CNPJ := copy(OnlyNumber(ACBrCTe1.WebServices.Consulta.CTeChave),7,14)
             else
             begin
               if not ValidarCNPJ(Cmd.Params(2)) then
                 raise Exception.Create('CNPJ '+Cmd.Params(2)+' inválido.')
             end;

             infEvento.cOrgao := StrToIntDef(copy(OnlyNumber(ACBrCTe1.WebServices.Consulta.CTeChave),1,2),0);
             infEvento.dhEvento := now;
             infEvento.tpEvento := teCancelamento;
             infEvento.chCTe := ACBrCTe1.WebServices.Consulta.CTeChave;
             infEvento.detEvento.nProt := ACBrCTe1.WebServices.Consulta.Protocolo;
             infEvento.detEvento.xJust := Cmd.Params(1);
           end;

           try
             ACBrCTe1.EnviarEvento(StrToIntDef(Cmd.Params(3),1));

             Cmd.Resposta := ACBrCTe1.WebServices.EnvEvento.EventoRetorno.xMotivo+sLineBreak+
                             '[CANCELAMENTO]'+sLineBreak+
                             'Versao='+ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.verAplic+sLineBreak+
                             'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.TpAmb)+sLineBreak+
                             'VerAplic='+ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.VerAplic+sLineBreak+
                             'CStat='+IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat)+sLineBreak+
                             'XMotivo='+ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.XMotivo+sLineBreak+
                             'CUF='+IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cOrgao)+sLineBreak+
                             'ChCTe='+ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.chCTe+sLineBreak+
                             'DhRecbto='+DateTimeToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento)+sLineBreak+
                             'NProt='+ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt+sLineBreak+
                             'tpEvento='+TpEventoToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.tpEvento)+sLineBreak+
                             'xEvento='+ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xEvento+sLineBreak+
                             'nSeqEvento='+IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nSeqEvento)+sLineBreak+
                             'CNPJDest='+ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.CNPJDest+sLineBreak+
                             'emailDest='+ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.emailDest+sLineBreak+
                             'XML='+ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.XML+sLineBreak;
          except
             raise Exception.Create(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.xMotivo);
          end;
         end
        else if Cmd.Metodo = 'imprimirdacte' then
         begin

           ACBrCTe1.Conhecimentos.Clear;
           PathsCTe := TStringList.Create;
           try
             PathsCTe.Append(Cmd.Params(0));
             PathsCTe.Append(PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             try
               CarregarDFe(PathsCTe, ArqCTe, tDFeCTe);
             except
             end;
           finally
             PathsCTe.Free;
           end;

           if NaoEstaVazio(Cmd.Params(1)) then
             ACBrCTe1.DACTe.Impressora := Cmd.Params(1)
           else
             ACBrCTe1.DACTe.Impressora := cbxImpressora.Text;

           if NaoEstaVazio(Cmd.Params(2)) then
             ACBrCTe1.DACTe.NumCopias := StrToIntDef(Cmd.Params(2),1)
           else
             ACBrCTe1.DACTe.NumCopias := StrToIntDef(edtNumCopia.Text,1);

           if NaoEstaVazio(Cmd.Params(3)) then
             ACBrCTe1.DACTe.ProtocoloCTE := Cmd.Params(3);
			  
	   if Cmd.Params(4) = '1' then
	     ACBrCTe1.DACTe.CTeCancelada := True;

           try
             AntesDeImprimir(ACBrCTe1.DACTe.MostrarPreview);
             ACBrCTe1.Conhecimentos.Imprimir;
           finally
             DepoisDeImprimir;
           end;

           Cmd.Resposta := 'Dacte Impresso com sucesso';

	   ACBrCTe1.DACTe.CTeCancelada := False;
         end

        else if Cmd.Metodo = 'imprimirdactepdf' then
         begin
           ACBrCTe1.Conhecimentos.Clear;
           CarregarDFe(Cmd.Params(0), ArqCTe, tDFeCTe);

           if NaoEstaVazio(Cmd.Params(1)) then
             ACBrCTe1.DACTe.ProtocoloCTE := Cmd.Params(1);

           try
	     if Cmd.Params(2) = '1' then
               ACBrCTe1.DACTe.CTeCancelada := True;
				
             ACBrCTe1.Conhecimentos.ImprimirPDF;
             ArqPDF := OnlyNumber(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.ID)+'-cte.pdf';
             Cmd.Resposta := 'Arquivo criado em: '+ PathWithDelim(ACBrCTe1.DACTe.PathPDF)+ArqPDF;
							  
	     ACBrCTe1.DACTe.CTeCancelada := False;
           except
              raise Exception.Create('Erro ao criar o arquivo PDF');
           end;
         end

        else if ( Cmd.Metodo = 'imprimirevento' ) or ( Cmd.Metodo = 'imprimireventopdf' ) then
         begin
           ACBrCTe1.EventoCTe.Evento.Clear;
           PathsCTe := TStringList.Create;
           try
             PathsCTe.Append(Cmd.Params(0));
             PathsCTe.Append(PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             try
               CarregarDFe(PathsCTe, ArqCTe, tDFeEventoCTe);
             except
             end;
           finally
             PathsCTe.Free;
           end;

           ACBrCTe1.Conhecimentos.Clear;
           if NaoEstaVazio(Cmd.Params(1)) then
           begin
             PathsCTe := TStringList.Create;
             try
               PathsCTe.Append(Cmd.Params(1));
               PathsCTe.Append(PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
               try
                 CarregarDFe(PathsCTe, ArqCTe, tDFeCTe);
               except
               end;
             finally
               PathsCTe.Free;
             end;
           end;

           if Cmd.Metodo = 'imprimireventopdf' then
           begin
             try
               ACBrCTe1.ImprimirEventoPDF;
               ArqPDF := OnlyNumber(ACBrCTe1.EventoCTe.Evento[0].InfEvento.Id);
               ArqPDF := PathWithDelim(ACBrCTe1.DACTe.PathPDF)+ArqPDF+'-procEventoCTe.pdf';
               Cmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
             except
               raise Exception.Create('Erro ao criar o arquivo PDF');
             end;
           end
           else
           begin
             if NaoEstaVazio(Cmd.Params(2)) then
               ACBrCTe1.DACTe.Impressora := Cmd.Params(2)
             else
               ACBrCTe1.DACTe.Impressora := cbxImpressora.Text;

             if NaoEstaVazio(Cmd.Params(3)) then
               ACBrCTe1.DACTe.NumCopias := StrToIntDef(Cmd.Params(3),1)
             else
               ACBrCTe1.DACTe.NumCopias := StrToIntDef(edtNumCopia.Text,1);

             try
               AntesDeImprimir(ACBrCTe1.DACTE.MostrarPreview);
               ACBrCTe1.ImprimirEvento;
             finally
               DepoisDeImprimir;
             end;

             Cmd.Resposta := 'Evento Impresso com sucesso';
           end;
         end

        else if Cmd.Metodo = 'inutilizarcte' then
         begin                            //CNPJ         //Justificat   //Ano                    //Modelo                 //Série                  //Num.Inicial            //Num.Final
           ACBrCTe1.WebServices.Inutiliza(Cmd.Params(0), Cmd.Params(1), StrToInt(Cmd.Params(2)), StrToInt(Cmd.Params(3)), StrToInt(Cmd.Params(4)), StrToInt(Cmd.Params(5)), StrToInt(Cmd.Params(6)));

           Cmd.Resposta := ACBrCTe1.WebServices.Inutilizacao.Msg+sLineBreak+
                           '[INUTILIZACAO]'+sLineBreak+
                           'Versao='+ACBrCTe1.WebServices.Inutilizacao.verAplic+sLineBreak+
                           'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Inutilizacao.TpAmb)+sLineBreak+
                           'VerAplic='+ACBrCTe1.WebServices.Inutilizacao.VerAplic+sLineBreak+
                           'CStat='+IntToStr(ACBrCTe1.WebServices.Inutilizacao.CStat)+sLineBreak+
                           'XMotivo='+ACBrCTe1.WebServices.Inutilizacao.XMotivo+sLineBreak+
                           'CUF='+IntToStr(ACBrCTe1.WebServices.Inutilizacao.CUF)+sLineBreak+
                           'DhRecbto='+DateTimeToStr(ACBrCTe1.WebServices.Inutilizacao.DhRecbto)+sLineBreak+
                           'NProt='+ACBrCTe1.WebServices.Inutilizacao.Protocolo+sLineBreak;
         end
        else if ( Cmd.Metodo = 'imprimirinutilizacao' ) or ( Cmd.Metodo = 'imprimirinutilizacaopdf' ) then
         begin
           PathsCTe := TStringList.Create;
           try
             PathsCTe.Append(Cmd.Params(0));
             PathsCTe.Append(PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             PathsCTe.Append(PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)+'-inu.xml');
             try
               CarregarDFe(PathsCTe, ArqCTe, tDFeInutCTe);
             except
             end;
           finally
             PathsCTe.Free;
           end;

           bMostrarPreview := ( (Cmd.Params(3) = '1') and (Cmd.Metodo <> 'imprimirinutilizacaopdf') );
           ACBrCTe1.DACTE := ACBrCTeDACTeRL1;

           if Cmd.Metodo = 'imprimirinutilizacaopdf' then
           begin
             try
                ACBrCTe1.ImprimirInutilizacaoPDF;
                ArqPDF := OnlyNumber(ACBrCTe1.InutCTe.ID);
                ArqPDF := PathWithDelim(ACBrCTe1.DACTE.PathPDF)+ArqPDF+'-procInutCTe.pdf';
                Cmd.Resposta := 'Arquivo criado em: ' + ArqPDF ;
             except
                raise Exception.Create('Erro ao criar o arquivo PDF');
             end;
           end
           else
           begin
             if NaoEstaVazio(Cmd.Params(1)) then
               ACBrCTe1.DACTE.Impressora := Cmd.Params(1)
             else
             begin
               if rgModoImpressaoEvento.ItemIndex = 0 then
                 ACBrCTe1.DACTE.Impressora := cbxImpressora.Text
               else
                 ACBrCTe1.DACTE.Impressora := cbxImpressoraNFCe.Text;
             end;

             if NaoEstaVazio(Cmd.Params(2)) then
                ACBrCTe1.DACTE.NumCopias := StrToIntDef(Cmd.Params(2),1);

             try
               AntesDeImprimir(bMostrarPreview);
               ACBrCTe1.ImprimirInutilizacao;
             finally
               DepoisDeImprimir;
             end;

             Cmd.Resposta := 'Inutilização Impressa com sucesso';
           end;
         end

        else if Cmd.Metodo = 'enviarcte' then
         begin
           ACBrCTe1.Conhecimentos.Clear;
           CarregarDFe(Cmd.Params(0), ArqCTe, tDFeCTe);
           ACBrCTe1.Conhecimentos.GerarCTe;

           if Cmd.Params(2) <> '0' then
              ACBrCTe1.Conhecimentos.Assinar;

           ACBrCTe1.Conhecimentos.Validar;

           if not(ACBrCTe1.WebServices.StatusServico.Executar) then
            raise Exception.Create(ACBrCTe1.WebServices.StatusServico.Msg);

           if Trim(OnlyNumber(Cmd.Params(1))) = '' then
              ACBrCTe1.WebServices.Enviar.Lote := '1'
           else
              ACBrCTe1.WebServices.Enviar.Lote := OnlyNumber(Cmd.Params(1)); //StrToIntDef( OnlyNumber(Cmd.Params(1)),1);

           ACBrCTe1.WebServices.Enviar.Executar;

           Cmd.Resposta :=  ACBrCTe1.WebServices.Enviar.Msg+sLineBreak+
                            '[ENVIO]'+sLineBreak+
                            'Versao='+ACBrCTe1.WebServices.Enviar.verAplic+sLineBreak+
                            'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Enviar.TpAmb)+sLineBreak+
                            'VerAplic='+ACBrCTe1.WebServices.Enviar.VerAplic+sLineBreak+
                            'CStat='+IntToStr(ACBrCTe1.WebServices.Enviar.CStat)+sLineBreak+
                            'XMotivo='+ACBrCTe1.WebServices.Enviar.XMotivo+sLineBreak+
                            'CUF='+IntToStr(ACBrCTe1.WebServices.Enviar.CUF)+sLineBreak+
                            'NRec='+ACBrCTe1.WebServices.Enviar.Recibo+sLineBreak+
                            'DhRecbto='+DateTimeToStr( ACBrCTe1.WebServices.Enviar.dhRecbto)+sLineBreak+
                            'TMed='+IntToStr( ACBrCTe1.WebServices.Enviar.tMed)+sLineBreak;

           if (ACBrCTe1.Configuracoes.Geral.ModeloDF <> moCTeOS) then
           begin
             ACBrCTe1.WebServices.Retorno.Recibo := ACBrCTe1.WebServices.Enviar.Recibo;
             ACBrCTe1.WebServices.Retorno.Executar;

             Cmd.Resposta :=  Cmd.Resposta+
                              ACBrCTe1.WebServices.Retorno.Msg+sLineBreak+
                              '[RETORNO]'+sLineBreak+
                              'Versao='+ACBrCTe1.WebServices.Retorno.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Retorno.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrCTe1.WebServices.Retorno.VerAplic+sLineBreak+
                              'NRec='+ACBrCTe1.WebServices.Retorno.CteRetorno.nRec+sLineBreak+
                              'CStat='+IntToStr(ACBrCTe1.WebServices.Retorno.CStat)+sLineBreak+
                              'XMotivo='+ACBrCTe1.WebServices.Retorno.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrCTe1.WebServices.Retorno.CUF)+sLineBreak;
           end;

           for I:= 0 to ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Count-1 do
            begin
              for J:= 0 to ACBrCTe1.Conhecimentos.Count-1 do
              begin
                if 'CTe'+ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].chCTe = ACBrCTe1.Conhecimentos.Items[j].CTe.infCTe.Id  then
                begin
                  Cmd.Resposta := Cmd.Resposta+
                             '[CTE'+Trim(IntToStr(ACBrCTe1.Conhecimentos.Items[J].CTe.Ide.nCT))+']'+sLineBreak+
                             'Versao='+ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].verAplic+sLineBreak+
                             'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].tpAmb)+sLineBreak+
                             'VerAplic='+ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].verAplic+sLineBreak+
                             'CStat='+IntToStr(ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].cStat)+sLineBreak+
                             'XMotivo='+ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].xMotivo+sLineBreak+
                             'CUF='+IntToStr(ACBrCTe1.WebServices.Retorno.CteRetorno.cUF)+sLineBreak+
                             'ChCTe='+ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].chCTe+sLineBreak+
                             'DhRecbto='+DateTimeToStr(ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].dhRecbto)+sLineBreak+
                             'NProt='+ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].nProt+sLineBreak+
                             'DigVal='+ACBrCTe1.WebServices.Retorno.CteRetorno.ProtCTe.Items[i].digVal+sLineBreak;
                  break;
                end;
              end;

              if NaoEstaVazio(Cmd.Params(4)) then
                 ACBrCTe1.DACTe.Impressora := Cmd.Params(4)
              else
                 ACBrCTe1.DACTe.Impressora := cbxImpressora.Text;

              if ACBrCTe1.Conhecimentos.Items[i].Confirmado and (Cmd.Params(3) = '1') then
              begin
                try
                  AntesDeImprimir(ACBrCTe1.DACTe.MostrarPreview);
                  ACBrCTe1.Conhecimentos.Items[i].Imprimir;
                finally
                  DepoisDeImprimir;
                end;
              end;
            end;
         end
         
        else if (Cmd.Metodo = 'recibocte')then
         begin
           ACBrCTe1.WebServices.Recibo.Recibo := Cmd.Params(0);
           if not(ACBrCTe1.WebServices.Recibo.Executar) then
             raise Exception.Create(ACBrCTe1.WebServices.Recibo.xMotivo);

           Cmd.Resposta :=  Cmd.Resposta+
                            ACBrCTe1.WebServices.Recibo.Msg+sLineBreak+
                           '[RETORNO]'+sLineBreak+
                           'Versao='+ACBrCTe1.WebServices.Recibo.verAplic+sLineBreak+
                           'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Recibo.TpAmb)+sLineBreak+
                           'VerAplic='+ACBrCTe1.WebServices.Recibo.VerAplic+sLineBreak+
                           'NRec='+ACBrCTe1.WebServices.Recibo.Recibo+sLineBreak+
                           'CStat='+IntToStr(ACBrCTe1.WebServices.Recibo.CStat)+sLineBreak+
                           'XMotivo='+ACBrCTe1.WebServices.Recibo.XMotivo+sLineBreak+
                           'CUF='+IntToStr(ACBrCTe1.WebServices.Recibo.CUF)+sLineBreak+
                           'ChCTe='+ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[0].chCTe+sLineBreak+
                           'NProt='+ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[0].nProt+sLineBreak+
                           'MotivoCTe='+ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[0].xMotivo+sLineBreak;

                           for I:= 0 to ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Count-1 do
                            begin
                              Cmd.Resposta := Cmd.Resposta+
                                '[CTE'+Trim(IntToStr(StrToInt(copy(ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].chCTe,26,9))))+']'+sLineBreak+
                                'Versao='+ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].verAplic+sLineBreak+
                                'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].tpAmb)+sLineBreak+
                                'VerAplic='+ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].verAplic+sLineBreak+
                                'CStat='+IntToStr(ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].cStat)+sLineBreak+
                                'XMotivo='+ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].xMotivo+sLineBreak+
                                'CUF='+IntToStr(ACBrCTe1.WebServices.Recibo.CTeRetorno.cUF)+sLineBreak+
                                'ChCTe='+ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].chCTe+sLineBreak+
                                'DhRecbto='+DateTimeToStr(ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].dhRecbto)+sLineBreak+
                                'NProt='+ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].nProt+sLineBreak+
                                'DigVal='+ACBrCTe1.WebServices.Recibo.CTeRetorno.ProtCTe.Items[i].digVal+sLineBreak;
                            end;

           if ACBrCTe1.Configuracoes.Geral.Salvar then
            begin
              Cmd.Resposta :=  Cmd.Resposta+
              'Arquivo='+ACBrCTe1.Configuracoes.Arquivos.PathSalvar+Cmd.Params(0)+'-pro-rec.xml';
            end;
         end

        else if (Cmd.Metodo = 'consultacadastro')then
         begin
           ACBrCTe1.WebServices.ConsultaCadastro.UF   := Cmd.Params(0);
           if Cmd.Params(2) = '1' then
              ACBrCTe1.WebServices.ConsultaCadastro.IE := Cmd.Params(1)
           else
            begin
              if Length(Cmd.Params(1)) > 11 then
                 ACBrCTe1.WebServices.ConsultaCadastro.CNPJ := Cmd.Params(1)
              else
                 ACBrCTe1.WebServices.ConsultaCadastro.CPF := Cmd.Params(1);
            end;
            ACBrCTe1.WebServices.ConsultaCadastro.Executar;

            Cmd.Resposta :=  Cmd.Resposta+
                             ACBrCTe1.WebServices.ConsultaCadastro.Msg+sLineBreak+
                             'VerAplic='+ACBrCTe1.WebServices.ConsultaCadastro.verAplic+sLineBreak+
                             'cStat='+IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.cStat)+sLineBreak+
                             'xMotivo='+ACBrCTe1.WebServices.ConsultaCadastro.xMotivo+sLineBreak+
                             'DhCons='+DateTimeToStr(ACBrCTe1.WebServices.ConsultaCadastro.DhCons)+sLineBreak+
                             'cUF='+IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.cUF)+sLineBreak+
                             'IE='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].IE+sLineBreak+
                             'CNPJ='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].CNPJ+sLineBreak+
                             'CPF='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].CPF+sLineBreak+
                             'UF='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].UF+sLineBreak+
                             'cSit='+IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].cSit)+sLineBreak+
                             'xNome='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xNome+sLineBreak+
                             'xFant='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xFant+sLineBreak+
                             'xRegApur='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xRegApur+sLineBreak+
                             'CNAE='+inttostr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].CNAE)+sLineBreak+
                             'dIniAtiv='+FormatDateBr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].dIniAtiv)+sLineBreak+
                             'dUltSit='+FormatDateBr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].dUltSit)+sLineBreak+
                             'dBaixa='+FormatDateBr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].dBaixa)+sLineBreak+
                             'xLgr='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xLgr+sLineBreak+
                             'nro='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].nro+sLineBreak+
                             'xCpl='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xCpl+sLineBreak+
                             'xBairro='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xBairro+sLineBreak+
                             'cMun='+inttostr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].cMun)+sLineBreak+
                             'xMun='+ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xMun+sLineBreak+
                             'CEP='+inttostr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].CEP)+sLineBreak;

         end

        else if (Cmd.Metodo = 'criarcte')      or (Cmd.Metodo = 'criarenviarcte') or
                (Cmd.Metodo = 'criarctesefaz') or (Cmd.Metodo = 'criarenviarctesefaz') or
                (Cmd.Metodo = 'adicionarcte')  or (Cmd.Metodo = 'adicionarctesefaz') or
                (Cmd.Metodo = 'enviarlotecte') then
         begin
           if (Cmd.Metodo = 'criarcte') or (Cmd.Metodo = 'criarenviarcte') or
              (Cmd.Metodo = 'adicionarcte') then
              GerarIniCTe( Cmd.Params(0)  );


           if (Cmd.Metodo = 'adicionarcte')  or (Cmd.Metodo = 'adicionarctesefaz') then
            begin
              ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(1)));
              ACBrCTe1.Conhecimentos.GerarCTe;
              Alertas := ACBrCTe1.Conhecimentos.Items[0].Alertas;
              ACBrCTe1.Conhecimentos.Assinar;
              ACBrCTe1.Conhecimentos.Validar;
              ArqCTe := PathWithDelim(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(1)))+OnlyNumber(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.ID)+'-cte.xml';
              ACBrCTe1.Conhecimentos.GravarXML(ExtractFilePath(ArqCTe));
              if not FileExists(ArqCTe) then
                 raise Exception.Create('Não foi possível criar o arquivo '+ArqCTe);
            end

           else if (Cmd.Metodo = 'criarcte')  or (Cmd.Metodo = 'criaresefaz') or
           (Cmd.Metodo = 'criarenviarcte') or (Cmd.Metodo = 'criarenviarctesefaz') then
            begin
              Salva := ACBrCTe1.Configuracoes.Geral.Salvar;
              if not Salva then
               begin
                ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
                ACBrCTe1.Configuracoes.Arquivos.PathSalvar := PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs';
               end;
              ACBrCTe1.Conhecimentos.GerarCTe;
              Alertas := ACBrCTe1.Conhecimentos.Items[0].Alertas;
              ACBrCTe1.Conhecimentos.Assinar;
              ACBrCTe1.Conhecimentos.Validar;
              ArqCTe := PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+OnlyNumber(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.ID)+'-cte.xml';
              ACBrCTe1.Conhecimentos.GravarXML(ArqCTe);
              if not FileExists(ArqCTe) then
                raise Exception.Create('Não foi possível criar o arquivo '+ArqCTe);
            end;

           Cmd.Resposta := ArqCTe;
           if Alertas <> '' then
              Cmd.Resposta :=  Cmd.Resposta+sLineBreak+'Alertas:'+Alertas;
           if ((Cmd.Metodo = 'criarcte') or (Cmd.Metodo = 'criarctesefaz')) and (Cmd.Params(1) = '1') then
            begin
              SL := TStringList.Create;
              SL.LoadFromFile(ArqCTe);
              Cmd.Resposta :=  Cmd.Resposta+sLineBreak+SL.Text;
              SL.Free;
            end;

           if (Cmd.Metodo = 'criarenviarcte') or (Cmd.Metodo = 'criarenviarctesefaz') or (Cmd.Metodo = 'enviarlotecte') then
            begin
              //Carregar Notas quando enviar lote
              if (Cmd.Metodo = 'enviarlotecte')   then
               begin
                 if not DirectoryExists(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(0))) then
                    raise Exception.Create('Diretório não encontrado:'+PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(0)))
                 else
                  begin
                    ACBrCTe1.Conhecimentos.Clear;
                    RetFind := SysUtils.FindFirst( PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+Cmd.Params(0)+PathDelim+'*-cte.xml', faAnyFile, SearchRec);
                    if (RetFind = 0) then
                     begin
                       while RetFind = 0 do
                        begin
                           ACBrCTe1.Conhecimentos.LoadFromFile(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+Cmd.Params(0)+PathDelim+SearchRec.Name);
                           RetFind := FindNext(SearchRec);
                        end;
                        ACBrCTe1.Conhecimentos.GerarCTe;
                        ACBrCTe1.Conhecimentos.Assinar;
                        ACBrCTe1.Conhecimentos.Validar;
                     end
                    else
                       raise Exception.Create('Não foi encontrada nenhuma nota para o Lote: '+Cmd.Params(0) );
                  end;
               end;

                 if not(ACBrCTe1.WebServices.StatusServico.Executar) then
                  raise Exception.Create(ACBrCTe1.WebServices.StatusServico.Msg);

                 if (Cmd.Metodo = 'criarenviarcte') or (Cmd.Metodo = 'criarenviarctesefaz') then
                  begin
                    if Trim(OnlyNumber(Cmd.Params(1))) = '' then
                       ACBrCTe1.WebServices.Enviar.Lote := '1'
                    else
                       ACBrCTe1.WebServices.Enviar.Lote := OnlyNumber(Cmd.Params(1)); //StrToIntDef( OnlyNumber(Cmd.Params(1)),1);
                  end
                 else
                  begin
                    if Trim(OnlyNumber(Cmd.Params(0))) = '' then
                       ACBrCTe1.WebServices.Enviar.Lote := '1'
                    else
                       ACBrCTe1.WebServices.Enviar.Lote := OnlyNumber(Cmd.Params(0)); //StrToIntDef( OnlyNumber(Cmd.Params(0)),1);
                  end;
                 ACBrCTe1.WebServices.Enviar.Executar;

                 Cmd.Resposta :=  ACBrCTe1.WebServices.Enviar.Msg+sLineBreak+
                                 '[ENVIO]'+sLineBreak+
                                 'Versao='+ACBrCTe1.WebServices.Enviar.verAplic+sLineBreak+
                                 'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Enviar.TpAmb)+sLineBreak+
                                 'VerAplic='+ACBrCTe1.WebServices.Enviar.VerAplic+sLineBreak+
                                 'CStat='+IntToStr(ACBrCTe1.WebServices.Enviar.CStat)+sLineBreak+
                                 'XMotivo='+ACBrCTe1.WebServices.Enviar.XMotivo+sLineBreak+
                                 'CUF='+IntToStr(ACBrCTe1.WebServices.Enviar.CUF)+sLineBreak+
                                 'NRec='+ACBrCTe1.WebServices.Enviar.Recibo+sLineBreak+
                                 'DhRecbto='+DateTimeToStr(ACBrCTe1.WebServices.Enviar.dhRecbto)+sLineBreak+
                                 'TMed='+IntToStr(ACBrCTe1.WebServices.Enviar.TMed)+sLineBreak+
                                 'Msg='+ACBrCTe1.WebServices.Enviar.Msg+sLineBreak;

                 if (ACBrCTe1.Configuracoes.Geral.ModeloDF <> moCTeOS) then
                 begin
                   ACBrCTe1.WebServices.Retorno.Recibo := ACBrCTe1.WebServices.Enviar.Recibo;
                   ACBrCTe1.WebServices.Retorno.Executar;

                   Cmd.Resposta :=  Cmd.Resposta+
                                    ACBrCTe1.WebServices.Retorno.Msg+sLineBreak+
                                    '[RETORNO]'+sLineBreak+
                                    'Versao='+ACBrCTe1.WebServices.Retorno.verAplic+sLineBreak+
                                    'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Retorno.TpAmb)+sLineBreak+
                                    'VerAplic='+ACBrCTe1.WebServices.Retorno.VerAplic+sLineBreak+
                                    'NRec='+ACBrCTe1.WebServices.Retorno.CteRetorno.nRec+sLineBreak+
                                    'CStat='+IntToStr(ACBrCTe1.WebServices.Retorno.CStat)+sLineBreak+
                                    'XMotivo='+ACBrCTe1.WebServices.Retorno.XMotivo+sLineBreak+
                                    'CUF='+IntToStr(ACBrCTe1.WebServices.Retorno.CUF)+sLineBreak;
                 end;
                 for I:= 0 to ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Count-1 do
                  begin
                   for J:= 0 to ACBrCTe1.Conhecimentos.Count-1 do
                    begin
                     if 'CTe'+ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].chCTe = ACBrCTe1.Conhecimentos.Items[j].CTe.infCTe.Id  then
                      begin
                        Cmd.Resposta := Cmd.Resposta+
                                   '[CTE'+Trim(IntToStr(ACBrCTe1.Conhecimentos.Items[j].CTe.Ide.nCT))+']'+sLineBreak+
                                   'Versao='+ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].verAplic+sLineBreak+
                                   'TpAmb='+TpAmbToStr(ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].tpAmb)+sLineBreak+
                                   'VerAplic='+ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].verAplic+sLineBreak+
                                   'CStat='+IntToStr(ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].cStat)+sLineBreak+
                                   'XMotivo='+ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].xMotivo+sLineBreak+
                                   'CUF='+IntToStr(ACBrCTe1.WebServices.Retorno.CteRetorno.cUF)+sLineBreak+
                                   'ChCTe='+ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].chCTe+sLineBreak+
                                   'DhRecbto='+DateTimeToStr(ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].dhRecbto)+sLineBreak+
                                   'NProt='+ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].nProt+sLineBreak+
                                   'DigVal='+ACBrCTe1.WebServices.Retorno.CTeRetorno.ProtCTe.Items[i].digVal+sLineBreak+
                                   'Arquivo='+PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+OnlyNumber(ACBrCTe1.Conhecimentos.Items[j].CTe.infCTe.ID)+'-CTe.xml'+sLineBreak;

                        ACBrCTe1.DACTe.Impressora := cbxImpressora.Text;
                        if ACBrCTe1.Conhecimentos.Items[i].Confirmado and (Cmd.Params(2) = '1') then
                         begin
                           try
                             AntesDeImprimir((Cmd.Params(2) = '1') and ACBrCTe1.DACTe.MostrarPreview);
                             ACBrCTe1.Conhecimentos.Items[i].Imprimir;
                           finally
                             DepoisDeImprimir;
                           end;
                         end;

                        break;
                      end;
                    end;
                  end;

            end;
         end

        else if (Cmd.Metodo = 'cartadecorrecao')then
         begin
           Cmd.Resposta := '';
           ACBrCTe1.EventoCTe.Evento.Clear;

           GerarCTeIniEvento( Cmd.Params(0) );

           ACBrCTe1.EnviarEvento(ACBrCTe1.EventoCTe.idLote);

           if ACBrCTe1.Configuracoes.Geral.Salvar then
            begin
              Cmd.Resposta :=  Cmd.Resposta+
              'Arquivo='+ACBrCTe1.WebServices.EnvEvento.ArqResp;
            end;
           Cmd.Resposta := Cmd.Resposta+sLineBreak+
                           'idLote='   +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.idLote)+sLineBreak+
                           'tpAmb='    +TpAmbToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.tpAmb)+sLineBreak+
                           'verAplic=' +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.verAplic+sLineBreak+
                           'cOrgao='   +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.cOrgao)+sLineBreak+
                           'cStat='    +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.cStat)+sLineBreak+
                           'xMotivo='  +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.xMotivo+sLineBreak;

           for I:= 0 to ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Count-1 do
            begin

              Cmd.Resposta := Cmd.Resposta+sLineBreak+
               '[EVENTO'+Trim(IntToStrZero(I+1,3))+']'+sLineBreak+
               'id='        +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.Id+sLineBreak+
               'tpAmb='     +TpAmbToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.tpAmb)+sLineBreak+
               'verAplic='  +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.verAplic+sLineBreak+
               'cOrgao='    +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.cOrgao)+sLineBreak+
               'cStat='     +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.cStat)+sLineBreak+
               'xMotivo='   +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.xMotivo+sLineBreak+
               'chCTe='     +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.chCTe+sLineBreak+
               'tpEvento='  +TpEventoToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.tpEvento)+sLineBreak+
               'xEvento='   +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.xEvento+sLineBreak+
               'nSeqEvento='+IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.nSeqEvento)+sLineBreak+
               'CNPJDest='  +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.CNPJDest+sLineBreak+
               'emailDest=' +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.emailDest+sLineBreak+
               'dhRegEvento='+DateTimeToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.dhRegEvento)+sLineBreak+
               'nProt='     +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.nProt+sLineBreak;
            end;
         end

        else if (Cmd.Metodo = 'enviarevento')then
         begin
           Cmd.Resposta := '';
           ACBrCTe1.EventoCTe.Evento.Clear;

           GerarCTeIniEvento( Cmd.Params(0) );

           ACBrCTe1.EnviarEvento(ACBrCTe1.EventoCTe.idLote);

           if ACBrCTe1.Configuracoes.Geral.Salvar then
            begin
              Cmd.Resposta :=  Cmd.Resposta+
              'Arquivo='+ACBrCTe1.WebServices.EnvEvento.ArqResp;
            end;
           Cmd.Resposta := Cmd.Resposta+sLineBreak+
                           'idLote='   +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.idLote)+sLineBreak+
                           'tpAmb='    +TpAmbToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.tpAmb)+sLineBreak+
                           'verAplic=' +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.verAplic+sLineBreak+
                           'cOrgao='   +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.cOrgao)+sLineBreak+
                           'cStat='    +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.cStat)+sLineBreak+
                           'xMotivo='  +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.xMotivo+sLineBreak;

           for I:= 0 to ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Count-1 do
            begin

              Cmd.Resposta := Cmd.Resposta+sLineBreak+
               '[EVENTO'+Trim(IntToStrZero(I+1,3))+']'+sLineBreak+
               'id='        +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.Id+sLineBreak+
               'tpAmb='     +TpAmbToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.tpAmb)+sLineBreak+
               'verAplic='  +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.verAplic+sLineBreak+
               'cOrgao='    +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.cOrgao)+sLineBreak+
               'cStat='     +IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.cStat)+sLineBreak+
               'xMotivo='   +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.xMotivo+sLineBreak+
               'chCTe='     +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.chCTe+sLineBreak+
               'tpEvento='  +TpEventoToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.tpEvento)+sLineBreak+
               'xEvento='   +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.xEvento+sLineBreak+
               'nSeqEvento='+IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.nSeqEvento)+sLineBreak+
               'CNPJDest='  +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.CNPJDest+sLineBreak+
               'emailDest=' +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.emailDest+sLineBreak+
               'dhRegEvento='+DateTimeToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.dhRegEvento)+sLineBreak+
               'nProt='     +ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.nProt+sLineBreak;
            end;
         end

        else if (Cmd.Metodo = 'distribuicaodfepornsu') or      //CTe.DistribuicaoDFePorNSU(cUF, cCNPJ, nNSU)
                (Cmd.Metodo = 'distribuicaodfeporultnsu') then  //CTe.DistribuicaoDFePorUltNSU(cUF, cCNPJ, nUltNSU)
         begin
           if not ValidarCNPJ(Cmd.Params(1)) then
              raise Exception.Create('CNPJ '+Cmd.Params(1)+' inválido.');

           try
              if Cmd.Metodo = 'distribuicaodfepornsu' then
                ACBrCTe1.DistribuicaoDFePorNSU(StrToIntDef(Cmd.Params(0),0),Cmd.Params(1),Cmd.Params(2))
              else if Cmd.Metodo = 'distribuicaodfeporultnsu' then
                ACBrCTe1.DistribuicaoDFePorUltNSU(StrToIntDef(Cmd.Params(0),0),Cmd.Params(1),Cmd.Params(2));

              if ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.cStat = 137 then
                sTemMais := '1'
             else
                sTemMais := '0'; //pog para facilitar a indicacao de continuidade

             Cmd.Resposta:= Cmd.Resposta+sLineBreak+
                            '[DISTRIBUICAODFE]'+sLineBreak+
                            'versao='  +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.versao+sLineBreak+
                            'tpAmb='   +TpAmbToStr(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.tpAmb)+sLineBreak+
                            'verAplic='+ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.verAplic+sLineBreak+
                            'cStat='   +IntToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.cStat)+sLineBreak+
                            'xMotivo=' +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo+sLineBreak+
                            'dhResp='  +DateTimeToStr(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.dhResp)+sLineBreak+
                            'indCont=' +sTemMais+sLineBreak+
                            'ultNSU='  +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.ultNSU+sLineBreak+
                            'maxNSU='  +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.maxNSU;
             J := 1;
             for i:= 0 to AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count-1 do
             begin
              if Trim(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.chCTe) <> '' then
                 begin
                      Cmd.Resposta := Cmd.Resposta+sLineBreak+
                       '[RESCTe'+Trim(IntToStrZero(J,3))+']'+sLineBreak+
                       'NSU='     +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].NSU+sLineBreak+
                       'chCTe='   +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.chCTe+sLineBreak+
                       'CNPJ='    +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.CNPJCPF+sLineBreak+
                       'xNome='   +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.xNome+sLineBreak+
                       'IE='      +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.IE+sLineBreak+
                       'dEmi='    +DateTimeToStr(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.dhEmi)+sLineBreak+
                       'vNF='     +FloatToStr(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.vNF)+sLineBreak+
                       'digVal='  +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.digVal+sLineBreak+
                       'dhRecbto='+DateTimeToStr(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.dhRecbto)+sLineBreak+
                       'cSitCTe=' +SituacaoDFeToStr(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.cSitCTe)+sLineBreak+
                       'nProt='   +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resCTe.nProt+sLineBreak+
                       'XML='     +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].XML;

                       inc(J);
                    end;
                 end;
              J := 1;
              for i:= 0 to AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count-1 do
               begin
                 if Trim(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.versao) <> '' then
                    begin
                     Cmd.Resposta := Cmd.Resposta+sLineBreak+
                      '[ProEve'     +Trim(IntToStrZero(J,3))+']'+sLineBreak+
                      'NSU='        +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].NSU+sLineBreak+
                      'chCTe='      +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.chCTe+sLineBreak+
                      'cOrgao='     +IntToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.cOrgao)+sLineBreak+
                      'CNPJ='       +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.CNPJ+sLineBreak+
                      'id='         +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.Id+sLineBreak+
                      'dhEvento='   +DateTimeToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.dhEvento)+sLineBreak+
                      'nSeqEvento=' +IntToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.nSeqEvento)+sLineBreak+
                      'tpAmb='      +TpAmbToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.tpAmb)+sLineBreak+
                      'tpEvento='   +TpEventoToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.tpEvento)+sLineBreak+
                      'verEvento='  +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.verEvento+sLineBreak+
                      'desEvento='  +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.descEvento+sLineBreak+
                      //descricao
                      'xJust='      +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.xJust+sLineBreak+
                      //emit
                      'EmiCnpj='    +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.emit.CNPJ+sLineBreak+
                      'EmiIe='      +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.emit.IE+sLineBreak+
                      'EmixNome='   +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.emit.xNome+sLineBreak+
                      //cte
                      'cteNProt='   +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.nProt+sLineBreak+
                      'cteChvCte='  +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.chCTe+sLineBreak+
                      'cteDhemi='   +DateTimeToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.dhEmi)+sLineBreak+
                      'cteModal='   +TpModalToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.modal)+sLineBreak+
                      'cteDhRebcto='+DateTimeToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.dhRecbto)+sLineBreak+
                      'XML='        +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].XML;

                     inc(J);
                    end;
                end;
              J := 1;
              for i:= 0 to AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count-1 do
               begin
                 if Trim(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.Id) <> '' then
                    begin
                     Cmd.Resposta := Cmd.Resposta+sLineBreak+
                     '[InfEve'     +Trim(IntToStrZero(J,3))+']'+sLineBreak+
                     'id='         +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.Id+sLineBreak+
                     'verAplic='   +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.verAplic+sLineBreak+
                     'tpAmb='      +TpAmbToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.tpAmb)+sLineBreak+
                     'cOrgao='     +IntToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.cOrgao)+sLineBreak+
                     'chCTe='      +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.chCTe+sLineBreak+
                     'cStat='      +IntToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.cStat)+sLineBreak+
                     'CnpjDest='   +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.CNPJDest+sLineBreak+
                     'cOrgaoAutor='+IntToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.cOrgaoAutor)+sLineBreak+
                     'tpEvento='   +TpEventoToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.tpEvento)+sLineBreak+
                     'nSeqEvento=' +IntToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.nSeqEvento)+sLineBreak+
                     'xEvento='    +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.xEvento+sLineBreak+
                     'xMotivo='    +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.xMotivo+sLineBreak+
                     'dhRegEvento='+DateTimeToStr(AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.dhRegEvento)+sLineBreak+
                     'emailDest='  +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.emailDest+sLineBreak+
                     'nProt='      +AcbrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.nProt+sLineBreak+
                     'XML='        +ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].XML;

                     inc(J);
                    end;
                end;
           except
             on E: Exception do
              begin
                raise Exception.Create(AcbrCTe1.WebServices.DistribuicaoDFe.Msg+sLineBreak+E.Message);
              end;
           end;
         end

        // Fim da implementação

        else if Cmd.Metodo = 'enviaremail' then
         begin
           ACBrCTe1.Conhecimentos.Clear;
           PathsCTe := TStringList.Create;
           try
             PathsCTe.Append(Cmd.Params(1));
             PathsCTe.Append(PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
             try
               CarregarDFe(PathsCTe, ArqCTe, tDFeCTe);
             except
             end;
           finally
             PathsCTe.Free;
           end;

           sMensagemEmail := TStringList.Create;
           CC := TstringList.Create;
           Anexos := TstringList.Create;

           try
             sMensagemEmail.Text := SubstituirVariaveis( mmEmailMsgCTe.Lines.Text );

             CC.DelimitedText := sLineBreak;
             CC.Text := StringReplace(Cmd.Params(4),';',sLineBreak,[rfReplaceAll]);

             Anexos.DelimitedText := sLineBreak;
             Anexos.Text := StringReplace(Cmd.Params(5),';',sLineBreak,[rfReplaceAll]);
             try
               ACBrCTe1.Conhecimentos.Items[0].EnviarEmail( Cmd.Params(0),
                                                           SubstituirVariaveis( IfThen(NaoEstaVazio(Cmd.Params(3)),Cmd.Params(3),edtEmailAssuntoCTe.Text) ),
                                                           sMensagemEmail
                                                           , (Cmd.Params(2) = '1')   // Enviar PDF junto
                                                           , CC    // Lista com emails que serão enviado cópias - TStrings
                                                           , Anexos); // Lista de anexos - TStrings


               Cmd.Resposta := 'Email enviado com sucesso';
             except
                on E: Exception do
                 begin
                   raise Exception.Create('Erro ao enviar email'+sLineBreak+E.Message);
                 end;
             end;
           finally
             CC.Free;
             Anexos.Free;
             sMensagemEmail.Free;
           end;
         end

        else if Cmd.Metodo = 'enviaremailevento' then
         begin
           ACBrCTe1.EventoCTe.Evento.Clear;
           PathsCTe := TStringList.Create;
           try
             PathsCTe.Append(Cmd.Params(1));
             PathsCTe.Append(PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
             try
               CarregarDFe(PathsCTe, ArqEvento, tDFeEventoCTe);
             except
             end;
           finally
             PathsCTe.Free;
           end;

           ACBrCTe1.Conhecimentos.Clear;
           if NaoEstaVazio(Cmd.Params(2)) then
           begin
             PathsCTe := TStringList.Create;
             try
               PathsCTe.Append(Cmd.Params(2));
               PathsCTe.Append(PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(2));
               try
                 CarregarDFe(PathsCTe, ArqCTe, tDFeCTe);
               except
               end;
             finally
               PathsCTe.Free;
             end;
           end;

           if (Cmd.Params(3) = '1') then
           begin
             try
               ACBrCTe1.ImprimirEventoPDF;

               ArqPDF := OnlyNumber(ACBrCTe1.EventoCTe.Evento[0].InfEvento.Id);
               ArqPDF := PathWithDelim(ACBrCTe1.DACTe.PathPDF)+ArqPDF+'-procEventoCTe.pdf';
             except
               raise Exception.Create('Erro ao criar o arquivo PDF');
             end;
           end;

           try
             CC:=TstringList.Create;
             CC.DelimitedText := sLineBreak;
             CC.Text := StringReplace(Cmd.Params(5),';',sLineBreak,[rfReplaceAll]);

             Anexos:=TstringList.Create;
             Anexos.DelimitedText := sLineBreak;
             Anexos.Text := StringReplace(Cmd.Params(6),';',sLineBreak,[rfReplaceAll]);

             if ( ArqEvento = '' ) then
             begin
               tipoEvento := ACBrCTe1.EventoCTe.Evento[0].InfEvento.tpEvento;
               ArqEvento  := ACBrCTe1.EventoCTe.ObterNomeArquivo(tipoEvento);
               ArqEvento  := PathWithDelim(ACBrCTe1.Configuracoes.Arquivos.GetPathEvento(ACBrCTe1.EventoCTe.Evento[0].InfEvento.tpEvento))+ArqEvento;
               ACBrCTe1.EventoCTe.Gerador.SalvarArquivo(ArqEvento);
             end;
             Anexos.Add(ArqEvento);

             if (Cmd.Params(3) = '1') then
               Anexos.Add(ArqPDF);

             try
               ACBrCTe1.EnviarEmail(Cmd.Params(0),
                 IfThen(NaoEstaVazio(Cmd.Params(4)),Cmd.Params(4),edtEmailAssuntoCTe.Text),
                 mmEmailMsgCTe.Lines,CC,Anexos);

               Cmd.Resposta := 'Email enviado com sucesso';
             except
               on E: Exception do
               begin
                 raise Exception.Create('Erro ao enviar email'+sLineBreak+E.Message);
               end;
             end;
           finally
             CC.Free;
             Anexos.Free;
           end;
         end

        else if Cmd.Metodo = 'setversaodf' then //CTe.SetVersaoDF(nVersao) 2.00 3.00
         begin
            VersaoDFCTe := StrToVersaoCTe(OK, Cmd.Params(0));
            if OK then
             begin
               ACBrCTe1.Configuracoes.Geral.VersaoDF :=  VersaoDFCTe;
               cbVersaoWSCTe.ItemIndex := cbVersaoWSCTe.Items.IndexOf(Cmd.Params(0)) ;
               SalvarIni;
             end
            else
              raise Exception.Create('Versão Inválida.');
         end

        else if Cmd.Metodo = 'setmodelodf' then //CTe.SetModeloDF(nModeloDF) 57 67
         begin
            ModeloDFCTe := StrToModeloCTe(OK, Cmd.Params(0));
            if OK then
               ACBrCTe1.Configuracoes.Geral.ModeloDF := ModeloDFCTe
            else
              raise Exception.Create('Modelo Inválido(57/67).');
         end

        else if Cmd.Metodo = 'setcertificado' then
         begin
           DoACBr(Cmd);
         end

        else if Cmd.Metodo = 'setambiente' then //1-Produção 2-Homologação
         begin
           if (StrToInt(Cmd.Params(0))>=1) and (StrToInt(Cmd.Params(0))<=2) then
            begin
              ACBrCTe1.Configuracoes.WebServices.Ambiente := StrToTpAmb(OK, Cmd.Params(0));
              rgTipoAmb.ItemIndex := ACBrCTe1.Configuracoes.WebServices.AmbienteCodigo-1;
              SalvarIni;
            end
           else
              raise Exception.Create('Ambiente Inválido.');
         end

        else if Cmd.Metodo = 'setlogomarca' then
        begin
          if FileExists(Cmd.Params(0)) then
          begin
            ACBrCTe1.DACTe.Logo       := Cmd.Params(0);
            edtLogoMarca.Text         := ACBrCTe1.DACTe.Logo;
            SalvarIni;
          end
          else
             raise Exception.Create('Arquivo não encontrado.');
        end

        else if Cmd.Metodo = 'setformaemissao' then 
         begin
           if cbModoEmissao.checked then
             exit;

           OK := False;
           FormaEmissao := StrToTpEmis(OK, Cmd.Params(0));

           if not OK then
             raise Exception.Create('Forma de Emissão Inválida: '+TpEmisToStr(FormaEmissao))
           else
           begin
             ACBrCTe1.Configuracoes.Geral.FormaEmissao := StrToTpEmis(OK, Cmd.Params(0));
             cbFormaEmissaoCTe.ItemIndex := ACBrCTe1.Configuracoes.Geral.FormaEmissaoCodigo-1;
             SalvarIni;
           end;
         end

        else if Cmd.Metodo = 'lercte' then
         begin
           try
             Cmd.Resposta := GerarCTeIni( Cmd.Params(0) );
           except
             on E: Exception do
             begin
               raise Exception.Create('Erro ao gerar INI da CTe.'+sLineBreak+E.Message);
             end;
           end;
         end

        else if Cmd.Metodo = 'ctetotxt' then  //1-Arquivo XML, 2-NomeArqTXT
         begin
           ACBrCTe1.Conhecimentos.Clear;
           CarregarDFe(Cmd.Params(0), ArqCTe, tDFeCTe);
           ACBrCTe1.Conhecimentos.Items[0].GravarXML(Cmd.Params(1));
           Cmd.Resposta := ChangeFileExt(ACBrCTe1.Conhecimentos.Items[0].NomeArq,'.txt');
         end

        else if Cmd.Metodo = 'savetofile' then
          DoACBr(Cmd)

        else if Cmd.Metodo = 'loadfromfile' then
          DoACBr(Cmd)

        else if Cmd.Metodo = 'fileexists' then
          DoACBr(Cmd)

        else if Cmd.Metodo = 'certificadodatavencimento' then
          Cmd.Resposta := DateToStr(ACBrCTe1.SSL.CertDataVenc)

        else if Cmd.Metodo = 'lerini' then // Recarrega configurações do arquivo INI
          LerIni

        else if Cmd.Metodo = 'gerarchave' then
         begin
           Chave := GerarChaveAcesso(
                      StrToInt(Cmd.Params(0)), //codigoUF
                      StringToDateTime(Cmd.Params(6)), //emissao
                      Cmd.Params(7), //CNPJ
                      StrToInt(Cmd.Params(3)), //serie
                      StrToInt(Cmd.Params(4)), //numero
                      StrToInt(Cmd.Params(5)), //tpemi
                      StrToInt(Cmd.Params(1)), //codigoNumerico
                      StrToInt(Cmd.Params(2)) ); //modelo
           Cmd.Resposta := Chave;
         end

        else if Cmd.Metodo = 'restaurar' then
           Restaurar1Click( frmAcbrMonitor )

        else if Cmd.Metodo = 'ocultar' then
           Ocultar1Click( frmAcbrMonitor )

        else if Cmd.Metodo = 'encerrarmonitor' then
           Application.Terminate

        else if Cmd.Metodo = 'ativo' then
           Cmd.Resposta := 'Ativo'
        else if Cmd.Metodo = 'versao' then
           Cmd.Resposta := sVersaoACBr
        else if Cmd.Metodo ='datahora' then
           Cmd.Resposta := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now )
        else if Cmd.Metodo ='data' then
           Cmd.Resposta := FormatDateTime('dd/mm/yyyy', Now )
        else if Cmd.Metodo ='hora' then
           Cmd.Resposta := FormatDateTime('hh:nn:ss', Now )
        else if pos('|'+Cmd.Metodo+'|', '|exit|bye|fim|sair|') > 0 then {fecha conexao}
         begin
           Cmd.Resposta := 'Obrigado por usar o ACBrNFeMonitor';
           mCmd.Lines.Clear;

           if Assigned( Conexao ) then
             Conexao.CloseSocket;
         end
        else //Else Final - Se chegou ate aqui, o comando é inválido
           raise Exception.Create('Comando inválido ('+Cmd.Comando+')');
     finally
        { Nada a fazer aqui por enquanto... :) }
     end;
  end;
end;

procedure GerarIniCTe( AStr: String );
var
  I, J, K, L : Integer;
  sSecao, sFim, sCampoAdic , sKey, versao: String;
  INIRec : TMemIniFile;
  OK     : boolean;
  fsICMSUFFim : TStrings;
begin
  INIRec := LerConverterIni(AStr);

  with FRMACBrMonitor do
  begin
   try
      ACBrCTe1.Conhecimentos.Clear;
      with ACBrCTe1.Conhecimentos.Add.CTe do
       begin
          versao        := INIRec.ReadString('infCTe','versao', VersaoCTeToStr(ACBrCTe1.Configuracoes.Geral.VersaoDF));
          infCTe.versao := StringToFloatDef( INIRec.ReadString('infCTe','versao', VersaoCTeToStr(ACBrCTe1.Configuracoes.Geral.VersaoDF)),0) ;

          versao := infCTe.VersaoStr;
          versao := StringReplace(versao,'versao="','',[rfReplaceAll,rfIgnoreCase]);
          versao := StringReplace(versao,'"','',[rfReplaceAll,rfIgnoreCase]);

          Ide.cCT         := INIRec.ReadInteger('ide','cCT', 0);
          Ide.cUF         := INIRec.ReadInteger('ide','cUF', 0);
          Ide.CFOP        := INIRec.ReadInteger('ide','CFOP',0);
          Ide.natOp       := INIRec.ReadString('ide','natOp',EmptyStr);
          Ide.forPag      := StrTotpforPag(OK,INIRec.ReadString('ide','forPag','0'));
          Ide.modelo      := INIRec.ReadInteger( 'ide','mod' ,55);
          ACBrCTe1.Configuracoes.Geral.ModeloDF := StrToModeloCTe(OK,IntToStr(Ide.modelo));
          ACBrCTe1.Configuracoes.Geral.VersaoDF := StrToVersaoCTe(OK,versao);
          Ide.serie       := INIRec.ReadInteger( 'ide','serie'  ,1);
          Ide.nCT         := INIRec.ReadInteger( 'ide','nCT' ,0);
          Ide.dhEmi       := StringToDateTime(INIRec.ReadString( 'ide','dhEmi','0'));
          Ide.tpImp       := StrToTpImp(  OK, INIRec.ReadString( 'ide','tpImp',TpImpToStr(ACBrCTe1.DACTe.TipoDACTE)));
          Ide.tpEmis      := StrToTpEmis( OK,INIRec.ReadString( 'ide','tpemis',IntToStr(ACBrCTe1.Configuracoes.Geral.FormaEmissaoCodigo)));
          Ide.tpCTe       := StrTotpCTe(OK,INIRec.ReadString('ide','tpCTe','0'));
          Ide.procEmi     := StrToProcEmi(OK,INIRec.ReadString( 'ide','procEmi','0'));
          Ide.verProc     := INIRec.ReadString(  'ide','verProc' ,'ACBrCTe' );

          Ide.refCTe      := INIRec.ReadString('ide','refCTe','');

          Ide.cMunEnv     := INIRec.ReadInteger('ide','cMunEnv',0);
          Ide.xMunEnv     := INIRec.ReadString('ide','xMunEnv','');
          Ide.UFEnv       := INIRec.ReadString('ide','UFEnv','');

          Ide.modal       := StrToTpModal(OK, INIRec.ReadString('ide','modal','01'));
          Ide.tpServ      := StrToTpServ(OK,INIRec.ReadString('ide','tpServ','0'));

          Ide.cMunIni     := INIRec.ReadInteger('ide','cMunIni',0);
          Ide.xMunIni     := INIRec.ReadString('ide','xMunIni','');
          Ide.UFIni       := INIRec.ReadString('ide','UFIni','');

          Ide.cMunFim     := INIRec.ReadInteger('ide','cMunFim',0);
          Ide.xMunFim     := INIRec.ReadString('ide','xMunFim','');
          Ide.UFFim       := INIRec.ReadString('ide','UFFim','');

          Ide.retira      := StrToTpRetira(OK,INIRec.ReadString('ide','retira','0'));
          if INIRec.ReadString('ide','xDetRetira','') <> '' then
            Ide.xDetRetira  := INIRec.ReadString('ide','xDetRetira','');

          Ide.dhCont      := StringToDateTime(INIRec.ReadString( 'ide','dhCont'  ,'0'));
          Ide.xJust       := INIRec.ReadString(  'ide','xJust' ,'' );

          Ide.toma03.Toma := StrToTpTomador(OK,INIRec.ReadString('toma3','toma','0'));

          Ide.indGlobalizado := StrToTIndicador(OK, INIRec.ReadString('ide','indGlobalizado','0'));
          Ide.indIEToma      := StrToindIEDest(OK, INIRec.ReadString('ide','indIEToma','1'));

          //CT-e OS
          I := 1;
          while true do
          begin
            sSecao := 'infPercurso'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString(sSecao,'UFPer','FIM');
            if (sFim = 'FIM') or (Length(sFim) <= 0) then
               break;
            with Ide.infPercurso.Add do
             begin
               UFPer   := sFim;
             end;
            Inc(I);
          end;

          if INIRec.ReadString('toma4','xNome','') <> '' then
           begin
             Ide.toma4.toma := StrToTpTomador(OK,INIRec.ReadString('toma4','toma','0'));
             Ide.toma4.CNPJCPF := INIRec.ReadString('toma4','CNPJCPF','');
             Ide.toma4.IE      := INIRec.ReadString('toma4','IE','');
             Ide.toma4.xNome   := INIRec.ReadString('toma4','xNome','');
             Ide.toma4.xFant   := INIRec.ReadString('toma4','xFant','');
             Ide.toma4.fone    := INIRec.ReadString('toma4','fone','');
             with Ide.toma4.enderToma do
              begin
                xLgr    := INIRec.ReadString('toma4','xLgr','');
                nro     := INIRec.ReadString('toma4','nro','');
                xCpl    := INIRec.ReadString('toma4','xCpl','');
                xBairro := INIRec.ReadString('toma4','xBairro','');
                cMun    := INIRec.ReadInteger('toma4','cMun',0);
                xMun    := INIRec.ReadString('toma4','xMun','');
                CEP     := INIRec.ReadInteger('toma4','CEP',0);
                UF      := INIRec.ReadString('toma4','UF','');
                cPais   := INIRec.ReadInteger('toma4','cPais',0);
                xPais   := INIRec.ReadString('toma4','xPais','');
              end;
             Ide.toma4.email   := INIRec.ReadString('toma4','email','');
           end;

          Compl.xCaracAd := INIRec.ReadString('compl','xCaracAd', '' );
          Compl.xCaracSer:= INIRec.ReadString('compl','xCaracSer',''  );
          Compl.xEmi     := INIRec.ReadString('compl','xEmi','');

          Compl.fluxo.xOrig  := INIRec.ReadString('compl','xOrig','');
          Compl.fluxo.xDest  := INIRec.ReadString('compl','xDest','');
          Compl.fluxo.xRota  := INIRec.ReadString('compl','xRota','');
          I := 1;
          while true do
           begin
             sSecao := 'PASS'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'xPass','FIM');
             if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;
             with compl.fluxo.pass.Add do
              begin
                xPass   := INIRec.ReadString(sSecao,'xPass','');
              end;
             Inc(I);
           end;

          Compl.Entrega.TipoData := StrToTpDataPeriodo(ok,INIRec.ReadString('compl','TipoData','0'));
          case Compl.Entrega.TipoData of
           tdSemData:
              begin
                Compl.Entrega.semData.tpPer := StrToTpDataPeriodo(ok,INIRec.ReadString('compl','tpPer','0'));
              end;
           tdNaData,tdAteData,tdApartirData:
              begin
                Compl.Entrega.comData.tpPer := StrToTpDataPeriodo(ok,INIRec.ReadString('compl','tpPer','0'));
                Compl.Entrega.comData.dProg := StringToDateTime(INIRec.ReadString('compl','dProg','0'));
              end;
           tdNoPeriodo:
              begin
                Compl.Entrega.noPeriodo.tpPer := StrToTpDataPeriodo(ok,INIRec.ReadString('compl','tpPer','0'));
                Compl.Entrega.noPeriodo.dIni  := StringToDateTime(INIRec.ReadString('compl','dIni','0'));
                Compl.Entrega.noPeriodo.dFim  := StringToDateTime(INIRec.ReadString('compl','dFim','0'));
              end;
          end;

          Compl.Entrega.TipoHora := StrToTpHorarioIntervalo(ok,INIRec.ReadString('compl','TipoHora','0'));
          case Compl.Entrega.TipoHora of
           thSemHorario:
              begin
                Compl.Entrega.semHora.tpHor := StrToTpHorarioIntervalo(ok,INIRec.ReadString('compl','tpHor','0'));
              end;
           thNoHorario,thAteHorario,thApartirHorario:
              begin
                Compl.Entrega.comHora.tpHor := StrToTpHorarioIntervalo(ok,INIRec.ReadString('compl','tpHor','0'));
                Compl.Entrega.comHora.hProg := StrToTime(INIRec.ReadString('compl','hProg','0'));
              end;
           thNoIntervalo:
              begin
                Compl.Entrega.noInter.tpHor := StrToTpHorarioIntervalo(ok,INIRec.ReadString('compl','tpHor','0'));
                Compl.Entrega.noInter.hIni  := StrToTime(INIRec.ReadString('compl','hIni','0'));
                Compl.Entrega.noInter.hFim  := StrToTime(INIRec.ReadString('compl','hFim','0'));
              end;
          end;

          Compl.origCalc  := INIRec.ReadString('compl','origCalc','');
          Compl.destCalc  := INIRec.ReadString('compl','destCalc','');
          Compl.xObs      := INIRec.ReadString('compl','xObs','');

          I := 1;
          while true do
           begin
             sSecao     := 'ObsCont'+IntToStrZero(I,3);
             sCampoAdic := INIRec.ReadString(sSecao,'Campo',INIRec.ReadString(sSecao,'xCampo','FIM'));
             if (sCampoAdic = 'FIM') or (Length(sCampoAdic) <= 0) then
                break;

             with Compl.ObsCont.Add do
              begin
                xCampo := sCampoAdic;
                xTexto := INIRec.ReadString( sSecao,'Texto',INIRec.ReadString( sSecao,'xTexto',''));
              end;
             Inc(I);
           end;

          I := 1;
          while true do
           begin
             sSecao     := 'ObsFisco'+IntToStrZero(I,3);
             sCampoAdic := INIRec.ReadString(sSecao,'Campo',INIRec.ReadString(sSecao,'xCampo','FIM'));
             if (sCampoAdic = 'FIM') or (Length(sCampoAdic) <= 0) then
                break;

             with Compl.ObsFisco.Add do
              begin
                xCampo := sCampoAdic;
                xTexto := INIRec.ReadString( sSecao,'Texto',INIRec.ReadString( sSecao,'xTexto',''));
              end;
             Inc(I);
           end;

          Emit.CNPJ   := INIRec.ReadString('emit','CNPJ','');
          Emit.IE     := INIRec.ReadString('emit','IE','');
          Emit.xNome  := INIRec.ReadString('emit','xNome','');
          Emit.xFant  := INIRec.ReadString('emit','xFant','');

          Emit.enderEmit.xLgr     := INIRec.ReadString('emit','xLgr','');
          Emit.enderEmit.nro      := INIRec.ReadString('emit','nro','');
          Emit.enderEmit.xCpl     := INIRec.ReadString('emit', 'xCpl','');
          Emit.enderEmit.xBairro  := INIRec.ReadString('emit','xBairro','');
          Emit.enderEmit.cMun     := INIRec.ReadInteger('emit','cMun',0);
          Emit.enderEmit.xMun     := INIRec.ReadString('emit','xMun','');
          Emit.enderEmit.CEP      := INIRec.ReadInteger('emit','CEP',0);
          Emit.enderEmit.UF       := INIRec.ReadString('emit','UF','');
          Emit.enderEmit.fone     := INIRec.ReadString('emit','fone','');

          ide.cUF   := INIRec.ReadInteger('ide','cUF', UFparaCodigo(Emit.enderEmit.UF));

          Rem.CNPJCPF             := INIRec.ReadString('rem','CNPJCPF','');
          Rem.IE                  := INIRec.ReadString('rem','IE','');
          Rem.xNome               := INIRec.ReadString('rem','xNome','');
          Rem.xFant               := INIRec.ReadString('rem','xFant','');
          Rem.fone                := INIRec.ReadString('rem','fone','');

          Rem.enderReme.xLgr      := INIRec.ReadString('rem','xLgr','');
          Rem.enderReme.nro       := INIRec.ReadString('rem','nro','');
          Rem.enderReme.xCpl      := INIRec.ReadString('rem','xCpl','');
          Rem.enderReme.xBairro   := INIRec.ReadString('rem','xBairro','');
          Rem.enderReme.cMun      := INIRec.ReadInteger('rem','cMun',0);
          Rem.enderReme.xMun      := INIRec.ReadString('rem','xMun','');
          Rem.enderReme.CEP       := INIRec.ReadInteger('rem','CEP',0);
          Rem.enderReme.UF        := INIRec.ReadString('rem','UF','');
          Rem.enderReme.cPais      := INIRec.ReadInteger( 'rem','cPais'    ,1058);
          Rem.enderReme.xPais      := INIRec.ReadString(  'rem','xPais'    ,'BRASIL');
          Rem.email                := INIRec.ReadString(  'rem','email' ,'');

        {$IFDEF PL_200}
          Rem.locColeta.CNPJCPF  := INIRec.ReadString('locColeta','CNPJCPF','');
          Rem.locColeta.xNome    := INIRec.ReadString('locColeta','xNome','');
          Rem.locColeta.xLgr     := INIRec.ReadString('locColeta','xLgr','');
          Rem.locColeta.nro      := INIRec.ReadString('locColeta','nro','');
          Rem.locColeta.xCpl     := INIRec.ReadString('locColeta','xCpl','');
          Rem.locColeta.xBairro  := INIRec.ReadString('locColeta','xBairro','');
          Rem.locColeta.cMun     := INIRec.ReadInteger('locColeta','cMun',0);
          Rem.locColeta.xMun     := INIRec.ReadString('locColeta','xMun','');
          Rem.locColeta.uf       := INIRec.ReadString('locColeta','UF','');
        {$ENDIF}

          //CT-e OS
          if INIRec.ReadString('toma','CNPJCPF','') <> '' then
          begin
            toma.CNPJCPF:= INIRec.ReadString('toma','CNPJCPF','');
            toma.IE     := INIRec.ReadString('toma','IE','');
            toma.xNome  := INIRec.ReadString('toma','xNome','');
            toma.xFant  := INIRec.ReadString('toma','xFant','');
            toma.email  := INIRec.ReadString('toma','email','');
            toma.fone   := INIRec.ReadString('toma','fone','');
            toma.endertoma.xLgr     := INIRec.ReadString('toma','xLgr','');
            toma.endertoma.nro      := INIRec.ReadString('toma','nro','');
            toma.endertoma.xCpl     := INIRec.ReadString('toma', 'xCpl','');
            toma.endertoma.xBairro  := INIRec.ReadString('toma','xBairro','');
            toma.endertoma.cMun     := INIRec.ReadInteger('toma','cMun',0);
            toma.endertoma.xMun     := INIRec.ReadString('toma','xMun','');
            toma.endertoma.CEP      := INIRec.ReadInteger('toma','CEP',0);
            toma.endertoma.UF       := INIRec.ReadString('toma','UF','');
            toma.endertoma.cPais    := INIRec.ReadInteger('toma','cPais',1058);
            toma.endertoma.xPais    := INIRec.ReadString('toma','xPais','');
          end;

          I := 1;
          while true do
           begin
             sSecao := 'infNF'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'mod','FIM');
             if sFim = 'FIM' then
                break;

           {$IFDEF PL_200}
             with infCTeNorm.infDoc.infNF.Add do
           {$ELSE}
             with Rem.InfNF.Add do
           {$ENDIF}
              begin
                nRoma   := INIRec.ReadString(sSecao,'nRoma','');
                nPed    := INIRec.ReadString(sSecao,'nPed','');
                modelo  := StrToModeloNF(OK,INIRec.ReadString(sSecao,'mod','01'));
                serie   := INIRec.ReadString(sSecao,'serie','');
                nDoc    := INIRec.ReadString(sSecao,'nDoc','');
                dEmi    := StringToDateTime(INIRec.ReadString( sSecao,'dEmi','0'));
                vBC     := StringToFloatDef( INIRec.ReadString(sSecao,'vBC','') ,0);
                vICMS   := StringToFloatDef( INIRec.ReadString(sSecao,'vICMS','') ,0);
                vBCST   := StringToFloatDef( INIRec.ReadString(sSecao,'vBCST','') ,0);
                vST     := StringToFloatDef( INIRec.ReadString(sSecao,'vST','') ,0);
                vProd   := StringToFloatDef( INIRec.ReadString(sSecao,'vProd','') ,0);
                vNF     := StringToFloatDef( INIRec.ReadString(sSecao,'vNF','') ,0);
                nCFOP   := INIRec.ReadInteger(sSecao,'nCFOP',0);
                nPeso   := StringToFloatDef( INIRec.ReadString(sSecao,'nPeso','') ,0);
                PIN     := INIRec.ReadString(sSecao,'PIN','');
                dPrev   := StringToDateTime(INIRec.ReadString( sSecao,'dPrev','0'));

              {$IFNDEF PL_200}
                locRet.CNPJCPF  := INIRec.ReadString(sSecao,'CNPJCPF','');
                locRet.xNome    := INIRec.ReadString(sSecao,'xNome','');
                locRet.xLgr     := INIRec.ReadString(sSecao,'xLgr','');
                locRet.nro      := INIRec.ReadString(sSecao,'nro','');
                locRet.xCpl     := INIRec.ReadString(sSecao,'xCpl','');
                locRet.xBairro  := INIRec.ReadString(sSecao,'xBairro','');
                locRet.cMun     := INIRec.ReadInteger(sSecao,'cMun',0);
                locRet.xMun     := INIRec.ReadString(sSecao,'xMun','');
                locRet.uf       := INIRec.ReadString(sSecao,'UF','');
              {$ENDIF}

              {$IFDEF PL_200}
                J := 1;
                while true do
                begin
                  sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3);
                  sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');

                  if sFim = 'FIM' then
                    break;

                  with infUnidTransp.Add do
                  begin
                    tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
                    idUnidTransp := INIRec.ReadString(sSecao,'idUnidTransp','');
                    qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                    K := 1;
                    while true do
                    begin
                      sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                      sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                      if sFim = 'FIM' then
                        break;

                      with lacUnidTransp.Add do
                      begin
                        nLacre := INIRec.ReadString(sSecao,'nLacre','');
                      end;

                      inc(K);
                    end;

                    K := 1;
                    while true do
                    begin
                      sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                      sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

                      if sFim = 'FIM' then
                        break;

                      with infUnidCarga.Add do
                      begin
                        tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                        idUnidCarga := INIRec.ReadString(sSecao,'idUnidCarga','');
                        qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                        L := 1;
                        while true do
                        begin
                          sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                          sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                          if sFim = 'FIM' then
                            break;

                          with lacUnidCarga.Add do
                          begin
                            nLacre := INIRec.ReadString(sSecao,'nLacre','');
                          end;

                          inc(L);
                        end;

                      end;
                      inc(K);
                    end;
                    inc(J);
                  end;
                end;

                J := 1;
                while true do
                begin
                  sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3);
                  sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

                  if sFim = 'FIM' then
                    break;

                  with infUnidCarga.Add do
                  begin
                    tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                    idUnidCarga := INIRec.ReadString(sSecao,'idUnidCarga','');
                    qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                    K := 1;
                    while true do
                    begin
                      sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                      sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                      if sFim = 'FIM' then
                        break;

                      with lacUnidCarga.Add do
                      begin
                        nLacre := INIRec.ReadString(sSecao,'nLacre','');
                      end;

                      inc(K);
                    end;
                  end;
                  inc(J);
                end;
              {$ENDIF}

              end;
             inc(I);
           end;

          I := 1;
          while true do
           begin
             sSecao := 'infNFe'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'Chave','FIM');
             if sFim = 'FIM' then
                break;

           {$IFDEF PL_200}
             with infCTeNorm.infDoc.infNFe.Add do
           {$ELSE}
             with Rem.InfNFe.Add do
           {$ENDIF}
              begin
                chave := INIRec.ReadString(sSecao,'chave','');
                PIN   := INIRec.ReadString(sSecao,'PIN','');
                dPrev := StringToDateTime(INIRec.ReadString( sSecao,'dPrev','0'));

              {$IFDEF PL_200}
                J := 1;
                while true do
                 begin
                   sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3);
                   sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');
                   if sFim = 'FIM' then
                      break;
                   with infUnidTransp.Add do
                    begin
                      tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
                      idUnidTransp := INIRec.ReadString(sSecao,'idUnidTransp','');
                      qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);
                      K := 1;
                      while true do
                       begin
                         sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                         sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                         if sFim = 'FIM' then
                            break;
                         with lacUnidTransp.Add do
                          begin
                            nLacre := INIRec.ReadString(sSecao,'nLacre','');
                          end;
                         inc(K);
                       end;

                      K := 1;
                      while true do
                       begin
                         sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                         sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');
                         if sFim = 'FIM' then
                            break;
                         with infUnidCarga.Add do
                          begin
                            tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                            idUnidCarga := INIRec.ReadString(sSecao,'idUnidCarga','');
                            qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);
                            L := 1;
                            while true do
                             begin
                               sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                               sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                               if sFim = 'FIM' then
                                  break;
                               with lacUnidCarga.Add do
                                begin
                                  nLacre := INIRec.ReadString(sSecao,'nLacre','');
                                end;
                               inc(L);
                             end;

                            inc(K);
                          end;
                       end;
                    end;
                   inc(J);
                 end;

                J := 1;
                while true do
                 begin
                   sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3);
                   sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');
                   if sFim = 'FIM' then
                      break;
                   with infUnidCarga.Add do
                    begin
                      tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                      idUnidCarga := INIRec.ReadString(sSecao,'idUnidCarga','');
                      qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);
                      K := 1;
                      while true do
                       begin
                         sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                         sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                         if sFim = 'FIM' then
                            break;
                         with lacUnidCarga.Add do
                          begin
                            nLacre := INIRec.ReadString(sSecao,'nLacre','');
                          end;
                         inc(K);
                       end;
                    end;
                   inc(J);
                 end;
              {$ENDIF}

              end;
             Inc(I);
           end;

          I := 1;
          while true do
           begin
             sSecao := 'infOutros'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'nDoc','FIM');
             if sFim = 'FIM' then
                break;

           {$IFDEF PL_200}
             with infCTeNorm.infDoc.infOutros.Add do
           {$ELSE}
             with Rem.InfOutros.Add do
           {$ENDIF}
              begin
                tpDoc      := StrToTpDocumento(OK,INIRec.ReadString(sSecao,'tpDoc','01'));
                descOutros := INIRec.ReadString(sSecao,'descOutros','');
                nDoc       := INIRec.ReadString(sSecao,'nDoc','');
                dEmi       := StringToDateTime(INIRec.ReadString( sSecao,'dEmi','0'));
                vDocFisc   := StringToFloatDef( INIRec.ReadString(sSecao,'vDocFisc','') ,0);
                dPrev      := StringToDateTime(INIRec.ReadString( sSecao,'dPrev','0'));

              {$IFDEF PL_200}
                J := 1;
                while true do
                 begin
                   sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3);
                   sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');
                   if sFim = 'FIM' then
                      break;
                   with infUnidTransp.Add do
                    begin
                      tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
                      idUnidTransp := INIRec.ReadString(sSecao,'idUnidTransp','');
                      qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);
                      K := 1;
                      while true do
                       begin
                         sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                         sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                         if sFim = 'FIM' then
                            break;
                         with lacUnidTransp.Add do
                          begin
                            nLacre := INIRec.ReadString(sSecao,'nLacre','');
                          end;
                         inc(K);
                       end;
                      K := 1;
                      while true do
                       begin
                         sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                         sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');
                         if sFim = 'FIM' then
                            break;
                         with infUnidCarga.Add do
                          begin
                            tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                            idUnidCarga := INIRec.ReadString(sSecao,'idUnidCarga','');
                            qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);
                            L := 1;
                            while true do
                             begin
                               sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                               sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                               if sFim = 'FIM' then
                                  break;
                               with lacUnidCarga.Add do
                                begin
                                  nLacre := INIRec.ReadString(sSecao,'nLacre','');
                                end;
                               inc(L);

                             end;
                           end;
                         inc(K);
                       end;
                    end;
                   inc(J);
                 end;

                J := 1;
                while true do
                 begin
                   sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3);
                   sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');
                   if sFim = 'FIM' then
                      break;
                   with infUnidCarga.Add do
                    begin
                      tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                      idUnidCarga := INIRec.ReadString(sSecao,'idUnidCarga','');
                      qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);
                      K := 1;
                      while true do
                       begin
                         sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                         sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                         if sFim = 'FIM' then
                            break;
                         with lacUnidCarga.Add do
                          begin
                            nLacre := INIRec.ReadString(sSecao,'nLacre','');
                          end;
                         inc(K);
                       end;
                    end;
                   inc(J);
                 end;
              {$ENDIF}

              end;
             Inc(I);
           end;

          Exped.CNPJCPF  := INIRec.ReadString('Exped','CNPJCPF','');
          Exped.IE       := INIRec.ReadString('Exped','IE','');
          Exped.xNome    := INIRec.ReadString('Exped','xNome','');
          Exped.fone     := INIRec.ReadString('Exped','fone','');
          Exped.email    := INIRec.ReadString('Exped','email','');

          Exped.enderExped.xLgr     := INIRec.ReadString('Exped','xLgr','');
          Exped.enderExped.nro      := INIRec.ReadString('Exped','nro','');
          Exped.enderExped.xCpl     := INIRec.ReadString('Exped','xCpl','');
          Exped.enderExped.xBairro  := INIRec.ReadString('Exped','xBairro','');
          Exped.enderExped.cMun     := INIRec.ReadInteger('Exped','cMun',0);
          Exped.enderExped.xMun     := INIRec.ReadString('Exped','xMun','');
          Exped.enderExped.CEP      := INIRec.ReadInteger('Exped', 'CEP',0);
          Exped.enderExped.UF       := INIRec.ReadString('Exped','UF','');
          Exped.enderExped.cPais    := INIRec.ReadInteger('Exped','cPais',1058);
          Exped.enderExped.xPais    := INIRec.ReadString('Exped', 'xPais', 'BRASIL');

          Receb.CNPJCPF  := INIRec.ReadString('Receb','CNPJCPF','');
          Receb.IE       := INIRec.ReadString('Receb','IE','');
          Receb.xNome    := INIRec.ReadString('Receb','xNome','');
          Receb.fone     := INIRec.ReadString('Receb','fone','');
          Receb.email    := INIRec.ReadString('Receb','email','');

          Receb.enderReceb.xLgr     := INIRec.ReadString('Receb','xLgr','');
          Receb.enderReceb.nro      := INIRec.ReadString('Receb','nro','');
          Receb.enderReceb.xCpl     := INIRec.ReadString('Receb','xCpl','');
          Receb.enderReceb.xBairro  := INIRec.ReadString('Receb','xBairro','');
          Receb.enderReceb.cMun     := INIRec.ReadInteger('Receb','cMun',0);
          Receb.enderReceb.xMun     := INIRec.ReadString('Receb','xMun','');
          Receb.enderReceb.CEP      := INIRec.ReadInteger('Receb', 'CEP',0);
          Receb.enderReceb.UF       := INIRec.ReadString('Receb','UF','');
          Receb.enderReceb.cPais    := INIRec.ReadInteger('Receb','cPais',1058);
          Receb.enderReceb.xPais    := INIRec.ReadString('Receb', 'xPais', 'BRASIL');


          Dest.CNPJCPF  := INIRec.ReadString('Dest','CNPJCPF','');
          Dest.IE       := INIRec.ReadString('Dest','IE','');
          Dest.xNome    := INIRec.ReadString('Dest','xNome','');
          Dest.fone     := INIRec.ReadString('Dest','fone','');
          Dest.email    := INIRec.ReadString('Dest','email','');
          Dest.ISUF     := INIRec.ReadString('Dest','ISUF','');

          Dest.enderDest.xLgr     := INIRec.ReadString('Dest','xLgr','');
          Dest.enderDest.nro      := INIRec.ReadString('Dest','nro','');
          if INIRec.ReadString('Dest','xCpl','') <> '' then
            Dest.enderDest.xCpl   := INIRec.ReadString('Dest','xCpl','');
          Dest.enderDest.xBairro  := INIRec.ReadString('Dest','xBairro','');
          Dest.enderDest.cMun     := INIRec.ReadInteger('Dest','cMun',0);
          Dest.enderDest.xMun     := INIRec.ReadString('Dest','xMun','');
          Dest.enderDest.CEP      := INIRec.ReadInteger('Dest', 'CEP',0);
          Dest.enderDest.UF       := INIRec.ReadString('Dest','UF','');
          Dest.enderDest.cPais    := INIRec.ReadInteger('Dest','cPais',1058);
          Dest.enderDest.xPais    := INIRec.ReadString('Dest', 'xPais', 'BRASIL');

          Dest.locEnt.CNPJCPF  := INIRec.ReadString('locEnt','CNPJCPF','');
          Dest.locEnt.xNome    := INIRec.ReadString('locEnt','xNome','');
          Dest.locEnt.xLgr     := INIRec.ReadString('locEnt','xLgr','');
          Dest.locEnt.nro      := INIRec.ReadString('locEnt','nro','');
          Dest.locEnt.xCpl     := INIRec.ReadString('locEnt','xCpl','');
          Dest.locEnt.xBairro  := INIRec.ReadString('locEnt','xBairro','');
          Dest.locEnt.cMun     := INIRec.ReadInteger('locEnt','cMun',0);
          Dest.locEnt.xMun     := INIRec.ReadString('locEnt','xMun','');
          Dest.locEnt.uf       := INIRec.ReadString('locEnt','UF','');

          vPrest.vTPrest := StringToFloatDef( INIRec.ReadString('vPrest','vTPrest','') ,0);
          vPrest.vRec    := StringToFloatDef( INIRec.ReadString('vPrest','vRec','') ,0);

          I := 1;
          while true do
          begin
            sSecao := 'Comp'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString(sSecao,'xNome','FIM');
            if sFim = 'FIM' then
               break;

            with vPrest.comp.Add do
            begin
              xNome := INIRec.ReadString(sSecao,'xNome','');
              vComp := StringToFloatDef( INIRec.ReadString(sSecao,'vComp','') ,0);
            end;
            Inc(I);
          end;

          Imp.vTotTrib   := StringToFloatDef( INIRec.ReadString('Imp','vComp',INIRec.ReadString('ICMS','vComp','')) ,0);
          Imp.infAdFisco := INIRec.ReadString('Imp','infAdFisco',INIRec.ReadString('ICMS','infAdFisco',''));


          if INIRec.ReadString('ICMS00', 'CST','') <> '' then
          begin
            Imp.ICMS.ICMS00.CST   := StrToCSTICMS(OK,INIRec.ReadString('ICMS00','CST','00'));
            imp.ICMS.SituTrib     := Imp.ICMS.ICMS00.CST;
            Imp.ICMS.ICMS00.vBC   := StringToFloatDef( INIRec.ReadString('ICMS00','vBC','') ,0);
            Imp.ICMS.ICMS00.pICMS := StringToFloatDef( INIRec.ReadString('ICMS00','pICMS','') ,0);
            Imp.ICMS.ICMS00.vICMS := StringToFloatDef( INIRec.ReadString('ICMS00','vICMS','') ,0);
          end;

          if INIRec.ReadString('ICMS20', 'CST','') <> '' then
          begin
            Imp.ICMS.ICMS20.CST     := StrToCSTICMS(OK,INIRec.ReadString('ICMS20','CST','00'));
            imp.ICMS.SituTrib       := Imp.ICMS.ICMS20.CST;
            Imp.ICMS.ICMS20.pRedBC  := StringToFloatDef( INIRec.ReadString('ICMS20','pRedBC','') ,0);
            Imp.ICMS.ICMS20.vBC     := StringToFloatDef( INIRec.ReadString('ICMS20','vBC','') ,0);
            Imp.ICMS.ICMS20.pICMS   := StringToFloatDef( INIRec.ReadString('ICMS20','pICMS','') ,0);
            Imp.ICMS.ICMS20.vICMS   := StringToFloatDef( INIRec.ReadString('ICMS20','vICMS','') ,0);
          end;

          if INIRec.ReadString('ICMS45','CST','') <> '' then
           begin
            Imp.ICMS.ICMS45.CST := StrToCSTICMS(OK,INIRec.ReadString('ICMS45','CST','40'));
            imp.ICMS.SituTrib   := Imp.ICMS.ICMS45.CST;
           end;

          if INIRec.ReadString('ICMS60', 'CST','') <> '' then
          begin
            Imp.ICMS.ICMS60.CST     := StrToCSTICMS(OK,INIRec.ReadString('ICMS60','CST','60'));
            imp.ICMS.SituTrib       := Imp.ICMS.ICMS60.CST;
            Imp.ICMS.ICMS60.vBCSTRet   := StringToFloatDef( INIRec.ReadString('ICMS60','vBCSTRet','') ,0);
            Imp.ICMS.ICMS60.vICMSSTRet := StringToFloatDef( INIRec.ReadString('ICMS60','vICMSSTRet','') ,0);
            Imp.ICMS.ICMS60.pICMSSTRet := StringToFloatDef( INIRec.ReadString('ICMS60','pICMSSTRet','') ,0);
            Imp.ICMS.ICMS60.vCred      := StringToFloatDef( INIRec.ReadString('ICMS60','vCred','') ,0);
          end;

          if INIRec.ReadString('ICMS90', 'CST','') <> '' then
          begin
            Imp.ICMS.ICMS90.CST     := StrToCSTICMS(OK,INIRec.ReadString('ICMS90','CST','90'));
            imp.ICMS.SituTrib       := Imp.ICMS.ICMS90.CST;
            Imp.ICMS.ICMS90.pRedBC  := StringToFloatDef( INIRec.ReadString('ICMS90','pRedBC','') ,0);
            Imp.ICMS.ICMS90.vBC     := StringToFloatDef( INIRec.ReadString('ICMS90','vBC','') ,0);
            Imp.ICMS.ICMS90.pICMS   := StringToFloatDef( INIRec.ReadString('ICMS90','pICMS','') ,0);
            Imp.ICMS.ICMS90.vICMS   := StringToFloatDef( INIRec.ReadString('ICMS90','vICMS','') ,0);
            Imp.ICMS.ICMS90.vCred   := StringToFloatDef( INIRec.ReadString('ICMS90','vCred','') ,0);
          end;

          if INIRec.ReadString('ICMSOutraUF', 'CST','') <> '' then
          begin
            Imp.ICMS.ICMSOutraUF.CST     := StrToCSTICMS(OK,INIRec.ReadString('ICMSOutraUF','CST','90'));
            imp.ICMS.SituTrib            := Imp.ICMS.ICMSOutraUF.CST;
            Imp.ICMS.ICMSOutraUF.pRedBCOutraUF  := StringToFloatDef( INIRec.ReadString('ICMSOutraUF','pRedBCOutraUF','') ,0);
            Imp.ICMS.ICMSOutraUF.vBCOutraUF     := StringToFloatDef( INIRec.ReadString('ICMSOutraUF','vBCOutraUF','') ,0);
            Imp.ICMS.ICMSOutraUF.pICMSOutraUF   := StringToFloatDef( INIRec.ReadString('ICMSOutraUF','pICMSOutraUF','') ,0);
            Imp.ICMS.ICMSOutraUF.vICMSOutraUF   := StringToFloatDef( INIRec.ReadString('ICMSOutraUF','vICMSOutraUF','') ,0);
          end;

          if INIRec.ReadInteger('ICMSSN', 'indSN',0) = 1 then
          begin
            imp.ICMS.SituTrib := cstICMSSN;
            Imp.ICMS.ICMSSN.indSN := INIRec.ReadInteger('ICMSSN', 'indSN',1);
          end;

          if StringToFloatDef( INIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0) <> 0 then
          begin
            Imp.ICMSUFFim.vBCUFFim       := StringToFloatDef( INIRec.ReadString('ICMSUFFim', 'vBCUFFim', ''), 0 );
            Imp.ICMSUFFim.pFCPUFFim      := StringToFloatDef( INIRec.ReadString('ICMSUFFim', 'pFCPUFFim', ''), 0 );
            Imp.ICMSUFFim.pICMSUFFim     := StringToFloatDef( INIRec.ReadString('ICMSUFFim', 'pICMSUFFim', ''), 0 );
            Imp.ICMSUFFim.pICMSInter     := StringToFloatDef( INIRec.ReadString('ICMSUFFim', 'pICMSInter', ''), 0 );
            Imp.ICMSUFFim.pICMSInterPart := StringToFloatDef( INIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0 );
            Imp.ICMSUFFim.vFCPUFFim      := StringToFloatDef( INIRec.ReadString('ICMSUFFim', 'vFCPUFFim', ''), 0 );
            Imp.ICMSUFFim.vICMSUFFim     := StringToFloatDef( INIRec.ReadString('ICMSUFFim', 'vICMSUFFim', ''), 0 );
            Imp.ICMSUFFim.vICMSUFIni     := StringToFloatDef( INIRec.ReadString('ICMSUFFim', 'vICMSUFIni', ''), 0 );
          end;

          //CT-e OS
          Imp.infTribFed.vPIS          := StringToFloatDef( INIRec.ReadString('infTribFed', 'vPIS', ''), 0);
          Imp.infTribFed.vCOFINS       := StringToFloatDef( INIRec.ReadString('infTribFed', 'vCOFINS', ''), 0);
          Imp.infTribFed.vIR           := StringToFloatDef( INIRec.ReadString('infTribFed', 'vIR', ''), 0);
          Imp.infTribFed.vINSS         := StringToFloatDef( INIRec.ReadString('infTribFed', 'vINSS', ''), 0);
          Imp.infTribFed.vCSLL         := StringToFloatDef( INIRec.ReadString('infTribFed', 'vCSLL', ''), 0);

          //CT-e OS
          infCTeNorm.infServico.xDescServ := INIRec.ReadString('infServico','xDescServ','');
          infCTeNorm.infServico.qCarga    := StringToFloatDef(INIRec.ReadString('infServico','qCarga',''), 0);

          //CT-e OS
          I := 1;
          while true do
           begin
             sSecao := 'infDocRef'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'nDoc','FIM');
             if sFim = 'FIM' then
                break;
             with infCTeNorm.infDocRef.Add do
             begin
               nDoc              := sFim;
               serie             := INIRec.ReadString(sSecao,'serie','');
               subserie          := INIRec.ReadString(sSecao,'subserie','');
               dEmi              := StringToDateTime(INIRec.ReadString(sSecao,'dEmi','0') );
               vDoc              := StringToFloatDef(INIRec.ReadString(sSecao,'vDoc','') ,0);
             end;
             Inc(I);
          end;

          //CT-e OS
          I := 1;
          while true do
          begin
            sSecao := 'seg'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString(sSecao,'respSeg','FIM');
            if sFim = 'FIM' then
               break;
            with infCTeNorm.seg.Add do
            begin
              respSeg              := StrToTpRspSeguro(OK, sFim);
              xSeg                 := INIRec.ReadString(sSecao,'xSeg','');
              nApol                := INIRec.ReadString(sSecao,'nApol','');
            end;
            Inc(I);
          end;

        {$IFDEF PL_200}
          infCTeNorm.infCarga.vCarga   := StringToFloatDef( INIRec.ReadString('infCarga','vCarga','') ,0);
          infCTeNorm.infCarga.proPred  := INIRec.ReadString('infCarga','proPred','');
          infCTeNorm.infCarga.xOutCat  := INIRec.ReadString('infCarga','xOutCat','');
          infCTeNorm.infCarga.vCargaAverb  := StringToFloatDef( INIRec.ReadString('infCarga','vCargaAverb','') ,0);

          I := 1;
          while true do
           begin
             sSecao := 'infQ'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'cUnid','FIM');
             if sFim = 'FIM' then
                break;
             with infCTeNorm.infCarga.infQ.Add do
             begin
               cUnid   := StrToUnidMed(OK, sFim);
               tpMed   := INIRec.ReadString(sSecao,'tpMed','');
               qCarga  := StringToFloatDef( INIRec.ReadString(sSecao,'qCarga','') ,0);
             end;
             Inc(I);
           end;
        {$ELSE}
          infCarga.vCarga   := StringToFloatDef( INIRec.ReadString('infCarga','vCarga','') ,0);
          infCarga.proPred  := INIRec.ReadString('infCarga','proPred','');
          infCarga.xOutCat  := INIRec.ReadString('infCarga','xOutCat','');

          I := 1;
          while true do
           begin
             sSecao := 'infQ'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'cUnid','FIM');
             if sFim = 'FIM' then
                break;
             with infCarga.infQ.Add do
             begin
               cUnid   := StrToUnidMed(OK, sFim);
               tpMed   := INIRec.ReadString(sSecao,'tpMed','');
               qCarga  := StringToFloatDef( INIRec.ReadString(sSecao,'qCarga','') ,0);
             end;
             Inc(I);
           end;
        {$ENDIF}

        {$IFNDEF PL_200}
          I := 1;
          while true do
           begin
             sSecao := 'contQt'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'nCont','FIM');
             if sFim = 'FIM' then
                break;
             with infCTeNorm.contQt.Add do
             begin
               nCont   := sFim;
               dPrev   := StringToDateTime(INIRec.ReadString( sSecao,'dPrev','0'));
               J := 1;
               while true do
                begin
                  sSecao := 'lacContQt'+IntToStrZero(I,3)+IntToStrZero(J,3);
                  sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                  if sFim = 'FIM' then
                    break;
                  with lacContQt.Add do
                   begin
                     nLacre := sFim;
                   end;
                  Inc(J);
                end;
             end;
             Inc(I);
           end;
        {$ENDIF}

          I := 1;
          while true do
           begin
             sSecao := 'docAnt'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'xNome','FIM');
             if sFim = 'FIM' then
                break;

           {$IFDEF PL_200}
             with infCTeNorm.docAnt.emiDocAnt.Add do
           {$ELSE}
             with infCTeNorm.emiDocAnt.Add do
           {$ENDIF}
             begin
               CNPJCPF := INIRec.ReadString(sSecao,'CNPJCPF','');
               IE      := INIRec.ReadString(sSecao,'IE','');
               UF      := INIRec.ReadString(sSecao,'UF','');
               xNome   := INIRec.ReadString(sSecao,'xNome','');
               J := 1;
               while true do
                begin
                  sSecao := 'idDocAnt'+IntToStrZero(I,3)+IntToStrZero(J,3);
                  sFim   := INIRec.ReadString(sSecao,'nDoc',INIRec.ReadString(sSecao,'chCTe','FIM'));
                  if sFim = 'FIM' then
                    break;
                  with idDocAnt.Add do
                   begin
                     if INIRec.ReadString(sSecao,'chCTe','') <> '' then
                       idDocAntEle.Add.chCTe := INIRec.ReadString(sSecao,'chCTe','')
                     else
                      begin
                        with idDocAntPap.Add do
                         begin
                           tpDoc  := StrToTpDocumentoAnterior(OK, INIRec.ReadString(sSecao,'tpDoc',''));
                           serie  := INIRec.ReadString(sSecao,'serie','');
                           subser := INIRec.ReadString(sSecao,'subser','');
                           nDoc   := INIRec.ReadInteger(sSecao,'nDoc',0);
                           dEmi   := StringToDateTime(INIRec.ReadString( sSecao,'dEmi','0'));
                         end
                      end;
                   end;

                  Inc(J);
                end;
             end;
             Inc(I);
           end;

          I := 1;
          while true do
           begin
             sSecao    := 'infSeg'+IntToStrZero(I,3);
             sFim   := INIRec.ReadString(sSecao,'respSeg','FIM');
             if sFim = 'FIM' then
                break;
                
           {$IFDEF PL_200}
             with infCTeNorm.seg.Add do
           {$ELSE}
             with infseg.Add do
           {$ENDIF}
              begin
                respSeg   := StrToTpRspSeguro(OK, INIRec.ReadString(sSecao,'respSeg',''));
                xSeg      := INIRec.ReadString(sSecao,'xSeg','');
                nApol     := INIRec.ReadString(sSecao,'nApol','');
                nAver     := INIRec.ReadString(sSecao,'nAver','');
                vCarga    := StringToFloatDef( INIRec.ReadString(sSecao,'vCarga','') ,0);
              end;
             Inc(I);
           end;

          if INIRec.ReadString('Rodo','RNTRC','') <> '' then
          begin
          {$IFDEF PL_200}
            infCTeNorm.Rodo.RNTRC := INIRec.ReadString('Rodo','RNTRC','');
            infCTeNorm.Rodo.dPrev := StringToDateTime(INIRec.ReadString( 'Rodo','dPrev','0'));
            infCTeNorm.Rodo.Lota  := StrToTpLotacao(OK,INIRec.ReadString('Rodo','lota',''));

            infCTeNorm.Rodo.CIOT  := INIRec.ReadString('Rodo','CIOT','');
          {$ELSE}
            Rodo.RNTRC := INIRec.ReadString('Rodo','RNTRC','');
            Rodo.dPrev := StringToDateTime(INIRec.ReadString( 'Rodo','dPrev','0'));
            Rodo.Lota  := StrToTpLotacao(OK,INIRec.ReadString('Rodo','lota',''));

            Rodo.CIOT  := INIRec.ReadString('Rodo','CIOT','');
          {$ENDIF}

            I := 1;
            while true do
             begin
               sSecao    := 'Occ'+IntToStrZero(I,3);
               sFim   := INIRec.ReadString(sSecao,'nOcc','FIM');
               if sFim = 'FIM' then
                  break;
                  
             {$IFDEF PL_200}
               with infCTeNorm.Rodo.Occ.Add do
             {$ELSE}
               with Rodo.Occ.Add do
             {$ENDIF}
                begin
                  serie     := INIRec.ReadString(sSecao,'serie','');
                  nOcc      := INIRec.ReadInteger(sSecao,'nOcc',0);
                  dEmi      := StringToDateTime(INIRec.ReadString( sSecao,'dEmi','0'));
                  EmiOCC.CNPJ := INIRec.ReadString(sSecao,'CNPJ','');
                  EmiOCC.cInt := INIRec.ReadString(sSecao,'cInt','');
                  EmiOCC.IE   := INIRec.ReadString(sSecao,'IE','');
                  EmiOCC.UF   := INIRec.ReadString(sSecao,'UF','');
                  EmiOCC.fone := INIRec.ReadString(sSecao,'fone','');
                end;
               Inc(I);
             end;

            I := 1;
            while true do
             begin
               sSecao    := 'valePed'+IntToStrZero(I,3);
               sFim   := INIRec.ReadString(sSecao,'CNPJForn','FIM');
               if sFim = 'FIM' then
                  break;
                  
             {$IFDEF PL_200}
               with infCTeNorm.Rodo.valePed.Add do
             {$ELSE}
               with Rodo.valePed.Add do
             {$ENDIF}
                begin
                  CNPJForn := INIRec.ReadString(sSecao,'CNPJForn','');
                  nCompra  := INIRec.ReadString(sSecao,'nCompra','');
                  CNPJPg   := INIRec.ReadString(sSecao,'CNPJPg','');
                end;
               Inc(I);
             end;

            I := 1;
            while true do
             begin
               sSecao    := 'veic'+IntToStrZero(I,3);
               sFim   := INIRec.ReadString(sSecao,'RENAVAM','FIM');
               if sFim = 'FIM' then
                  break;

             {$IFDEF PL_200}
               with infCTeNorm.Rodo.veic.Add do
             {$ELSE}
               with Rodo.veic.Add do
             {$ENDIF}
                begin
                  cInt     := INIRec.ReadString(sSecao,'cInt','');
                  RENAVAM  := INIRec.ReadString(sSecao,'RENAVAM','');
                  placa    := INIRec.ReadString(sSecao,'placa','');
                  tara     := INIRec.ReadInteger(sSecao,'tara',0);
                  capKG    := INIRec.ReadInteger(sSecao,'capKG',0);
                  capM3    := INIRec.ReadInteger(sSecao,'capM3',0);
                  tpProp   := StrToTpPropriedade(OK,INIRec.ReadString(sSecao,'tpProp',''));
                  tpVeic   := StrToTpVeiculo(OK,INIRec.ReadString(sSecao,'tpVeic',''));
                  tpRod    := StrToTpRodado(OK,INIRec.ReadString(sSecao,'tpRod',''));
                  tpCar    := StrToTpCarroceria(OK,INIRec.ReadString(sSecao,'tpCar',''));
                  UF       := INIRec.ReadString(sSecao,'UF','');
                  Prop.CNPJCPF := INIRec.ReadString(sSecao,'CNPJ','');
                  Prop.RNTRC   := INIRec.ReadString(sSecao,'RNTRC','');
                  Prop.xNome   := INIRec.ReadString(sSecao,'xNome','');
                  Prop.IE      := INIRec.ReadString(sSecao,'IE','');
                  Prop.UF      := INIRec.ReadString(sSecao,'PropUF',UF);
                  Prop.tpProp  := StrToTpProp(OK,INIRec.ReadString(sSecao,'ProptpProp',INIRec.ReadString(sSecao,'tpProp','')));
                end;
               Inc(I);
             end;

            I := 1;
            while true do
             begin
               sSecao    := 'lacre'+IntToStrZero(I,3);
               sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
               if sFim = 'FIM' then
                  break;

             {$IFDEF PL_200}
               with infCTeNorm.Rodo.lacRodo.Add do
             {$ELSE}
               with Rodo.Lacres.Add do
             {$ENDIF}
                begin
                  nLacre := sFim;
                end;
               Inc(I);
             end;

            I := 1;
            while true do
             begin
               sSecao    := 'moto'+IntToStrZero(I,3);
               sFim   := INIRec.ReadString(sSecao,'xNome','FIM');
               if sFim = 'FIM' then
                  break;

             {$IFDEF PL_200}
               with infCTeNorm.Rodo.moto.Add do
             {$ELSE}
               with Rodo.moto.Add do
             {$ENDIF}
                begin
                  xNome := sFim;
                  CPF   := INIRec.ReadString(sSecao,'CPF','');
                end;
               Inc(I);
             end;
          end;

          //Rodoviário CT-e OS
          sSecao := 'RodoOS';
          if  INIRec.ReadString(sSecao,'TAF', INIRec.ReadString(sSecao,'NroRegEstadual','') ) <> ''  then
          begin
            infCTeNorm.rodoOS.TAF            := INIRec.ReadString(sSecao,'TAF','');
            infCTeNorm.rodoOS.NroRegEstadual := INIRec.ReadString(sSecao,'NroRegEstadual','');

            I := 1;
            while true do
            begin
              sSecao    := 'veic'+IntToStrZero(I,3);
              sFim   := INIRec.ReadString(sSecao,'placa','FIM');
              if sFim = 'FIM' then
                 break;

              with infCTeNorm.rodoOS.veic do
              begin
                placa                    := sFim;
                RENAVAM                  := INIRec.ReadString(sSecao,'RENAVAM','');
                UF                       := INIRec.ReadString(sSecao,'UF','');
                prop.CNPJCPF             := INIRec.ReadString(sSecao,'CNPJCPF','');
                prop.TAF                 := INIRec.ReadString(sSecao,'TAF','');
                prop.NroRegEstadual      := INIRec.ReadString(sSecao,'NroRegEstadual','');
                prop.xNome               := INIRec.ReadString(sSecao,'xNome','');
                prop.IE                  := INIRec.ReadString(sSecao,'IE','');
                prop.UF                  := INIRec.ReadString(sSecao,'propUF','');
                prop.tpProp              := StrToTpProp(OK,INIRec.ReadString(sSecao,'ProptpProp',INIRec.ReadString(sSecao,'tpProp','')));

              end;
              Inc(I);
            end;
          end;

          if INIRec.ReadString('aereo','CL','') <> '' then
           begin
             sSecao := 'aereo';
           {$IFDEF PL_200}
             with infCTeNorm do
              begin
           {$ENDIF}
               Aereo.nMinu   := INIRec.ReadInteger(sSecao,'nMinu',0);
               Aereo.nOCA    := INIRec.ReadString(sSecao,'nOCA','');

              {$IFDEF PL_200}
               Aereo.dPrevAereo := StringToDateTime(INIRec.ReadString( sSecao,'dPrevAereo','0'));
              {$ELSE}
               Aereo.dPrev   := StringToDateTime(INIRec.ReadString( sSecao,'dPrev','0'));
              {$ENDIF}
              
               Aereo.xLAgEmi := INIRec.ReadString(sSecao,'xLAgEmi','');
               Aereo.IdT     := INIRec.ReadString(sSecao,'IdT','');

               Aereo.tarifa.CL   := INIRec.ReadString(sSecao,'nMinu','');
               Aereo.tarifa.cTar := INIRec.ReadString(sSecao,'cTar','');
               Aereo.tarifa.vTar := StringToFloatDef( INIRec.ReadString(sSecao,'vTar','') ,0);

               Aereo.natCarga.xDime    := INIRec.ReadString(sSecao,'xDime','');
               Aereo.natCarga.cIMP     := INIRec.ReadString(sSecao,'cIMP','');
               I := 1;
               while true do
                begin
                  sKey   := 'cInfManu'+IntToStrZero(I,3);
                  sFim   := INIRec.ReadString(sSecao,sKey,'FIM');
                  if sFim = 'FIM' then
                     break;

                  with Aereo.natCarga.cinfManu.Add do
                    nInfManu := StrToTpInfManu(Ok, sFim);

                  Inc(I);
                end;

           {$IFDEF PL_200}
              end;
           {$ENDIF}
           end;

          if INIRec.ReadString('aquav','xNavio','') <> '' then
           begin
             sSecao := 'aquav';
           {$IFDEF PL_200}
             with infCTeNorm do
              begin
           {$ENDIF}
               Aquav.vPrest   := StringToFloatDef( INIRec.ReadString(sSecao,'vPrest','') ,0);
               Aquav.vAFRMM   := StringToFloatDef( INIRec.ReadString(sSecao,'vAFRMM','') ,0);
               Aquav.nBooking := INIRec.ReadString( sSecao,'nBooking','0');
               Aquav.nCtrl    := INIRec.ReadString(sSecao,'nCtrl','');
               Aquav.xNavio   := INIRec.ReadString(sSecao,'xNavio','');

               Aquav.nViag    := INIRec.ReadString(sSecao,'nViag','');
               Aquav.direc    := StrToTpDirecao(OK,INIRec.ReadString(sSecao,'direc',''));
               Aquav.prtEmb   := INIRec.ReadString(sSecao,'prtEmb','');
               Aquav.prtTrans := INIRec.ReadString(sSecao,'prtTrans','');
               Aquav.prtDest  := INIRec.ReadString(sSecao,'prtDest','');
               Aquav.tpNav    := StrToTpNavegacao(OK,INIRec.ReadString(sSecao,'tpNav',''));
               Aquav.irin     := INIRec.ReadString(sSecao,'irin','');

               I := 1;
               while true do
                begin
                  sSecao    := 'balsa'+IntToStrZero(I,3);
                  sFim   := INIRec.ReadString(sSecao,'xBalsa','FIM');
                  if sFim = 'FIM' then
                     break;
                  with Aquav.balsa.Add do
                   begin
                     xBalsa := sFim;
                   end;
                  Inc(I);
                end;

              {$IFNDEF PL_200}
               I := 1;
               while true do
                begin
                  sSecao    := 'detCont'+IntToStrZero(I,3);
                  sFim   := INIRec.ReadString(sSecao,'nCont','FIM');
                  if sFim = 'FIM' then
                     break;
                  with Aquav.detCont.Add do
                   begin
                     nCont := sFim;
                     J := 1;
                     while true do
                      begin
                        sSecao := 'Lacre'+IntToStrZero(I,3)+IntToStrZero(J,3);
                        sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                        if (sFim = 'FIM') or (Length(sFim) <= 0)  then
                           break;

                        Lacre.Add.nLacre := sFim;
                        Inc(J);
                      end;

                     J := 1;
                     while true do
                      begin
                        sSecao := 'infNF'+IntToStrZero(I,3)+IntToStrZero(J,3);
                        sFim   := INIRec.ReadString(sSecao,'nDoc','FIM');
                        if (sFim = 'FIM') or (Length(sFim) <= 0)  then
                           break;

                        with infNFCont.Add do
                         begin
                           nDoc    := sFim;
                           serie   := INIRec.ReadString(sSecao,'serie','FIM');
                           unidRat := StringToFloatDef( INIRec.ReadString(sSecao,'unidRat','') ,0);
                         end;
                        Inc(J);
                      end;

                     J := 1;
                     while true do
                      begin
                        sSecao := 'infNFe'+IntToStrZero(I,3)+IntToStrZero(J,3);
                        sFim   := INIRec.ReadString(sSecao,'chave','FIM');
                        if (sFim = 'FIM') or (Length(sFim) <= 0)  then
                           break;

                        with infNFeCont.Add do
                         begin
                           chave   := sFim;
                           unidRat := StringToFloatDef( INIRec.ReadString(sSecao,'unidRat','') ,0);
                         end;
                        Inc(J);
                      end;
                   end;
                  Inc(I);

                end;
              {$ENDIF}

           {$IFDEF PL_200}
              end;
           {$ENDIF}
           end;

         if INIRec.ReadString('ferrov','tpTraf','') <> '' then
          begin
            sSecao := 'ferrov';
           {$IFDEF PL_200}
           with infCTeNorm do
            begin
           {$ENDIF}
              Ferrov.tpTraf := StrToTpTrafego(OK,INIRec.ReadString(sSecao,'tpTraf',''));
              Ferrov.trafMut.respFat := StrToTrafegoMutuo(OK,INIRec.ReadString(sSecao,'respFat',''));
              Ferrov.trafMut.ferrEmi := StrToTrafegoMutuo(OK,INIRec.ReadString(sSecao,'ferrEmi',''));
              Ferrov.fluxo  := INIRec.ReadString( sSecao,'fluxo','0');
              Ferrov.idTrem := INIRec.ReadString( sSecao,'idTrem','0');
              Ferrov.vFrete := StringToFloatDef( INIRec.ReadString(sSecao,'vFrete','') ,0);

             {$IFDEF PL_200}
              I := 1;
              while true do
               begin
                 sSecao    := 'ferroEnv'+IntToStrZero(I,3);
                 sFim   := INIRec.ReadString(sSecao,'CNPJ','FIM');
                 if sFim = 'FIM' then
                    break;

                 with Ferrov.ferroEnv.Add do
                  begin
                    CNPJ  := sFim;
                    IE    := INIRec.ReadString(sSecao,'IE','');
                    xNome := INIRec.ReadString(sSecao,'xNome','');

                    EnderFerro.xLgr    := INIRec.ReadString(sSecao,'xLgr','');
                    EnderFerro.nro     := INIRec.ReadString(sSecao,'nro','');
                    EnderFerro.xCpl    := INIRec.ReadString(sSecao, 'xCpl','');
                    EnderFerro.xBairro := INIRec.ReadString(sSecao,'xBairro','');
                    EnderFerro.cMun    := INIRec.ReadInteger(sSecao,'cMun',0);
                    EnderFerro.xMun    := INIRec.ReadString(sSecao,'xMun','');
                    EnderFerro.CEP     := INIRec.ReadInteger(sSecao,'CEP',0);
                    EnderFerro.UF      := INIRec.ReadString(sSecao,'UF','');
                  end;

                 Inc(I);
               end;
             {$ELSE}
              sSecao := 'ferroEnv';

              Ferrov.ferroEnv.CNPJ   := INIRec.ReadString(sSecao,'CNPJ','');
              Ferrov.ferroEnv.IE     := INIRec.ReadString(sSecao,'IE','');
              Ferrov.ferroEnv.xNome  := INIRec.ReadString(sSecao,'xNome','');

              Ferrov.ferroEnv.EnderFerro.xLgr     := INIRec.ReadString(sSecao,'xLgr','');
              Ferrov.ferroEnv.EnderFerro.nro      := INIRec.ReadString(sSecao,'nro','');
              Ferrov.ferroEnv.EnderFerro.xCpl     := INIRec.ReadString(sSecao, 'xCpl','');
              Ferrov.ferroEnv.EnderFerro.xBairro  := INIRec.ReadString(sSecao,'xBairro','');
              Ferrov.ferroEnv.EnderFerro.cMun     := INIRec.ReadInteger(sSecao,'cMun',0);
              Ferrov.ferroEnv.EnderFerro.xMun     := INIRec.ReadString(sSecao,'xMun','');
              Ferrov.ferroEnv.EnderFerro.CEP      := INIRec.ReadInteger(sSecao,'CEP',0);
              Ferrov.ferroEnv.EnderFerro.UF       := INIRec.ReadString(sSecao,'UF','');
             {$ENDIF}

              I := 1;
              while true do
               begin
                 sSecao    := 'detVag'+IntToStrZero(I,3);
                 sFim   := INIRec.ReadString(sSecao,'nVag','FIM');
                 if sFim = 'FIM' then
                    break;

                 with Ferrov.detVag.Add do
                  begin
                    nVag   := StrToInt(sFim);
                    cap    := StringToFloatDef( INIRec.ReadString(sSecao,'cap','') ,0);
                    tpVag  := INIRec.ReadString(sSecao,'tpVag','');
                    pesoR  := StringToFloatDef( INIRec.ReadString(sSecao,'pesoR','') ,0);
                    pesoBC := StringToFloatDef( INIRec.ReadString(sSecao,'pesoBC','') ,0);

                   {$IFNDEF PL_200}
                    J := 1;
                    while true do
                     begin
                       sSecao := 'lacDetVag'+IntToStrZero(I,3)+IntToStrZero(J,3);
                       sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                       if (sFim = 'FIM') or (Length(sFim) <= 0)  then
                          break;

                       lacDetVag.Add.nLacre := sFim;
                       Inc(J);
                     end;

                    J := 1;
                    while true do
                     begin
                       sSecao := 'contVag'+IntToStrZero(I,3)+IntToStrZero(J,3);
                       sFim   := INIRec.ReadString(sSecao,'nCont','FIM');
                       if (sFim = 'FIM') or (Length(sFim) <= 0)  then
                          break;

                       with contVag.Add do
                        begin
                          nCont    := sFim;
                          dPrev   := StringToDateTime(INIRec.ReadString( sSecao,'dPrev','0'));
                        end;
                       Inc(J);
                     end;

                    J := 1;
                    while true do
                     begin
                       sSecao := 'ratNF'+IntToStrZero(I,3)+IntToStrZero(J,3);
                       sFim   := INIRec.ReadString(sSecao,'nDoc','FIM');
                       if (sFim = 'FIM') or (Length(sFim) <= 0)  then
                          break;

                       with ratNF.Add do
                        begin
                          nDoc    := sFim;
                          serie   := INIRec.ReadString(sSecao,'serie','FIM');
                          pesoRat := StringToFloatDef( INIRec.ReadString(sSecao,'pesoRat','') ,0);
                        end;
                       Inc(J);
                     end;

                    J := 1;
                    while true do
                     begin
                       sSecao := 'ratNFe'+IntToStrZero(I,3)+IntToStrZero(J,3);
                       sFim   := INIRec.ReadString(sSecao,'chave','FIM');
                       if (sFim = 'FIM') or (Length(sFim) <= 0)  then
                          break;

                       with ratNFe.Add do
                        begin
                          chave   := sFim;
                          pesoRat := StringToFloatDef( INIRec.ReadString(sSecao,'pesoRat','') ,0);
                        end;
                       Inc(J);
                     end;
                   {$ENDIF}
                  end;
                 Inc(I);
               end;
           {$IFDEF PL_200}
            end;
           {$ENDIF}
          end;

         if INIRec.ReadString('duto','dIni','') <> '' then
          begin
            sSecao := 'duto';
           {$IFDEF PL_200}
            with infCTeNorm do
             begin
           {$ENDIF}
              duto.vTar := StringToFloatDef( INIRec.ReadString(sSecao,'pesoRat','') ,0);
              duto.dIni := StringToDateTime(INIRec.ReadString( sSecao,'dIni','0'));
              duto.dFim := StringToDateTime(INIRec.ReadString( sSecao,'dFim','0'));
           {$IFDEF PL_200}
             end;
           {$ENDIF}
          end;

         I := 1;
         while true do
          begin
            sSecao := 'peri'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString(sSecao,'nONU','FIM');
            if sFim = 'FIM' then
              break;

          {$IFDEF PL_200}
            with infCTeNorm.peri.Add do
          {$ELSE}
            with peri.Add do
          {$ENDIF}
             begin
               nONU        := sFim;
               xNomeAE     := INIRec.ReadString( sSecao,'xNomeAE','');
               xClaRisco   := INIRec.ReadString( sSecao,'xClaRisco','');
               grEmb       := INIRec.ReadString( sSecao,'grEmb','');
               qTotProd    := INIRec.ReadString( sSecao,'qTotProd','');
               qVolTipo    := INIRec.ReadString( sSecao,'qVolTipo','');
               pontoFulgor := INIRec.ReadString( sSecao,'pontoFulgor','');
             end;
            Inc(I);
          end;

         I := 1;
         while true do
          begin
            sSecao := 'veicNovos'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString(sSecao,'chassi','FIM');
            if sFim = 'FIM' then
              break;

          {$IFDEF PL_200}
            with infCTeNorm.veicNovos.Add do
          {$ELSE}
            with veicNovos.Add do
          {$ENDIF}
             begin
               chassi := sFim;
               cCor   := INIRec.ReadString( sSecao,'cCor','');
               xCor   := INIRec.ReadString( sSecao,'xCor','');
               cMod   := INIRec.ReadString( sSecao,'cMod','');
               vUnit  := StringToFloatDef( INIRec.ReadString(sSecao,'vUnit','') ,0);
               vFrete := StringToFloatDef( INIRec.ReadString(sSecao,'vFrete','') ,0);
             end;
            Inc(I);
          end;

       {$IFDEF PL_200}
         with infCTeNorm do
          begin
       {$ENDIF}
           cobr.Fat.nFat  := INIRec.ReadString( 'cobr','nFat','');
           cobr.Fat.vOrig := StringToFloatDef( INIRec.ReadString('cobr','vOrig','') ,0);
           cobr.Fat.vDesc := StringToFloatDef( INIRec.ReadString('cobr','vDesc','') ,0);
           cobr.Fat.vLiq  := StringToFloatDef( INIRec.ReadString('cobr','vLiq' ,'') ,0);
       {$IFDEF PL_200}
         end;
       {$ENDIF}

         I := 1;
         while true do
          begin
            sSecao := 'dup'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString(sSecao,'nDup','FIM');
            if (sFim = 'FIM') or (Length(sFim) <= 0) then
               break;

          {$IFDEF PL_200}
            with infCTeNorm.Cobr.Dup.Add do
          {$ELSE}
            with Cobr.Dup.Add do
          {$ENDIF}
             begin
               nDup  := sFim;
               dVenc := StringToDateTime(INIRec.ReadString( sSecao,'dVenc','0'));
               vDup  := StringToFloatDef(INIRec.ReadString(sSecao,'vDup','') ,0);
             end;
            Inc(I);
          end;

         if INIRec.ReadString( 'infCteSub','chCte','') <> '' then
          begin
          {$IFDEF PL_200}
            with infCTeNorm.infCteSub do
          {$ELSE}
            with infCTeSub do
          {$ENDIF}
             begin
               chCte := INIRec.ReadString( 'infCteSub','chCte','');

               tomaICMS.refNFe := INIRec.ReadString( 'infCteSub','refNFe','');

             {$IFDEF PL_200}
               tomaICMS.refNF.CNPJCPF := INIRec.ReadString( 'infCteSub','CNPJ','');
             {$ELSE}
               tomaICMS.refNF.CNPJ   := INIRec.ReadString( 'infCteSub','CNPJ','');
             {$ENDIF}
               tomaICMS.refNF.modelo := INIRec.ReadString( 'infCteSub','mod','');
               tomaICMS.refNF.serie  := INIRec.ReadInteger( 'infCteSub','serie',0);
               tomaICMS.refNF.subserie := INIRec.ReadInteger( 'infCteSub','subserie',0);
               tomaICMS.refNF.nro    := INIRec.ReadInteger( 'infCteSub','CNPJ',0);
               tomaICMS.refNF.valor  :=  StringToFloatDef(INIRec.ReadString('infCteSub','valor','') ,0);
               tomaICMS.refNF.dEmi   := StringToDateTime(INIRec.ReadString( 'infCteSub','dEmi','0'));

               tomaNaoICMS.refCteAnu := INIRec.ReadString( 'infCteSub','refCteAnu','');
             end;
          end;

         if INIRec.ReadString('infCteComp', 'chave', '') <> '' then
          begin
          {$IFDEF PL_200}
            infCTeComp.chave := INIRec.ReadString('infCteComp', 'chave', '');
          {$ENDIF}
          end;

         if INIRec.ReadString( 'infCteAnu','chCte','') <> '' then
          begin
          {$IFDEF PL_200}
            InfCTeAnu.chCTe := INIRec.ReadString( 'infCteAnu','chCte','');
            InfCTeAnu.dEmi  := StringToDateTime(INIRec.ReadString( 'infCteAnu','dEmi','0'));
          {$ELSE}
            InfCTeAnuEnt.chCTe := INIRec.ReadString( 'infCteAnu','chCte','');
            InfCTeAnuEnt.dEmi  := StringToDateTime(INIRec.ReadString( 'infCteAnu','dEmi','0'));
          {$ENDIF}
          end;

         {$IFDEF PL_200}
         I := 1;
         while true do
          begin
            sSecao := 'autXML' + IntToStrZero(I, 2);
            sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');
            if (sFim = 'FIM') or (Length(sFim) <= 0) then
               break;

            with autXML.Add do
             begin
               CNPJCPF := sFim;
             end;
            Inc(I);
          end;
         {$ENDIF}

       end;
   finally
      INIRec.Free;
   end;
  end;
end;

function GerarCTeIni( XML : String ): String;
var
  I : Integer;
  sSecao: String;
  INIRec : TMemIniFile;
  IniCTe : TStringList;
  LocCTeR : TCTeR;
begin
 INIRec := TMemIniFile.create( 'cte.ini' );
 FrmACBrMonitor.ACBrCTe1.Conhecimentos.Clear;
 if FilesExists(XML) then
    FRMACBrMonitor.ACBrCTe1.Conhecimentos.LoadFromFile(XML)
 else
  begin
    LocCTeR := TCTeR.Create(FRMACBrMonitor.ACBrCTe1.Conhecimentos.Add.CTe);
    try
       LocCTeR.Leitor.Arquivo := ConvertStrRecived( XML );
       LocCTeR.LerXml;
       FRMACBrMonitor.ACBrCTe1.Conhecimentos.Items[0].XML := LocCTeR.Leitor.Arquivo;
       FRMACBrMonitor.ACBrCTe1.Conhecimentos.GerarCTe;
    finally
       LocCTeR.Free;
    end;
  end;

 with FRMACBrMonitor do
  begin
   try
      with ACBrCTe1.Conhecimentos.Items[0].CTe do
       begin
          INIRec.WriteInteger('ide', 'cCT', Ide.cCT);
          INIRec.WriteInteger('ide', 'CFOP', Ide.CFOP);
          INIRec.WriteString('ide', 'natOp', Ide.natOp);
          INIRec.WriteString('ide', 'forPag', tpforPagToStr(Ide.forPag));
          INIRec.WriteInteger('ide', 'mod', Ide.modelo);
          INIRec.WriteInteger('ide', 'serie', Ide.serie);
          INIRec.WriteInteger('ide', 'nCT', Ide.nCT);
          INIRec.WriteString('ide', 'dhEmi', DateToStr(Ide.dhEmi));
          INIRec.WriteString('ide', 'tpImp', TpImpToStr(Ide.tpImp));
          INIRec.WriteString('ide', 'tpemis', TpEmisToStr(Ide.tpEmis));
          INIRec.WriteString('ide', 'procEmi', procEmiToStr(Ide.procEmi));
          INIRec.WriteString('ide', 'verProc', Ide.verProc);
          INIRec.WriteString('ide', 'dhCont', DateToStr(Ide.dhCont));
          INIRec.WriteString('ide', 'xJust', Ide.xJust);
          INIRec.WriteString('ide', 'tpCTe', tpCTePagToStr(Ide.tpCTe));
          INIRec.WriteString('ide', 'refCTe', Ide.refCTe);
          INIRec.WriteInteger('ide', 'cMunEnv', Ide.cMunEnv);
          INIRec.WriteString('ide', 'xMunEnv', Ide.xMunEnv);
          INIRec.WriteString('ide', 'UFEnv', Ide.UFEnv);
          INIRec.WriteString('ide', 'modal', TpModalToStr(Ide.modal));
          INIRec.WriteString('ide', 'tpServ', TpServPagToStr(Ide.tpServ));
          INIRec.WriteInteger('ide', 'cMunIni', Ide.cMunIni);
          INIRec.WriteString('ide', 'xMunIni', Ide.xMunIni);
          INIRec.WriteString('ide', 'UFIni', Ide.UFIni);
          INIRec.WriteInteger('ide', 'cMunFim', Ide.cMunFim);
          INIRec.WriteString('ide', 'xMunFim', Ide.xMunFim);
          INIRec.WriteString('ide', 'UFFim', Ide.UFFim);
          INIRec.WriteString('ide', 'retira', TpRetiraPagToStr(Ide.retira));
          INIRec.WriteString('ide', 'xDetRetira', Ide.xDetRetira);
          INIRec.WriteString('ide','indGlobalizado', TIndicadorToStr(Ide.indGlobalizado));
          INIRec.WriteString('ide','indIEToma', indIEDestToStr(Ide.indIEToma));

          INIRec.WriteString('toma3', 'toma', TpTomadorToStr(Ide.toma03.Toma));

          Ide.toma4.CNPJCPF := INIRec.ReadString('toma4','CNPJCPF','');
          Ide.toma4.IE      := INIRec.ReadString('toma4','IE','');
          Ide.toma4.xNome   := INIRec.ReadString('toma4','xNome','');
          Ide.toma4.xFant   := INIRec.ReadString('toma4','xFant','');
          Ide.toma4.fone    := INIRec.ReadString('toma4','fone','');
            with Ide.toma4.enderToma do
            begin
              xLgr    := INIRec.ReadString('toma4','xLgr','');
              nro     := INIRec.ReadString('toma4','nro','');
              xCpl    := INIRec.ReadString('toma4','xCpl','');
              xBairro := INIRec.ReadString('toma4','xBairro','');
              cMun    := INIRec.ReadInteger('toma4','cMun',0);
              xMun    := INIRec.ReadString('toma4','xMun','');
              CEP     := INIRec.ReadInteger('toma4','CEP',0);
              UF      := INIRec.ReadString('toma4','UF','');
              cPais   := INIRec.ReadInteger('toma4','cPais',0);
              xPais   := INIRec.ReadString('toma4','xPais','');
            end;
          Ide.toma4.email   := INIRec.ReadString('toma4','email','');

          INIRec.WriteString('ide', 'dhCont', DateToStr(Ide.dhCont));
          INIRec.WriteString('ide', 'xJust', Ide.xJust);

          INIRec.WriteString('compl', 'xCaracAd', compl.xCaracAd);
          INIRec.WriteString('compl', 'xCaracSer', compl.xCaracSer);
          INIRec.WriteString('compl', 'xEmi', compl.xEmi);

          INIRec.WriteString('compl', 'tpPer', TpDataPeriodoToStr(compl.Entrega.TipoData));
          INIRec.WriteString('compl', 'tpHor', TpHorarioIntervaloToStr(compl.Entrega.TipoHora));
          {ainda tem mais dados aqui}

          {...}
          INIRec.WriteString('compl', 'origCalc', compl.origCalc);
          INIRec.WriteString('compl', 'destCalc', compl.destCalc);
          INIRec.WriteString('compl', 'xObs', compl.xObs);

          INIRec.WriteString('emit', 'CNPJ', Emit.CNPJ);
          INIRec.WriteString('emit', 'IE', Emit.IE);
          INIRec.WriteString('emit', 'xNome', Emit.xNome);
          INIRec.WriteString('emit', 'xFant', Emit.xFant);

          INIRec.WriteString('emit', 'xLgr', Emit.enderEmit.xLgr);
          INIRec.WriteString('emit', 'nro', Emit.enderEmit.nro);
          INIRec.WriteString('emit', 'xCpl', Emit.enderEmit.xCpl);
          INIRec.WriteString('emit', 'xBairro', Emit.enderEmit.xBairro);
          INIRec.WriteInteger('emit', 'cMun', Emit.enderEmit.cMun);
          INIRec.WriteString('emit', 'xMun', Emit.enderEmit.xMun);
          INIRec.WriteInteger('emit', 'CEP', Emit.enderEmit.CEP);
          INIRec.WriteString('emit', 'UF', Emit.enderEmit.UF);

          INIRec.WriteString('emit', 'fone', Emit.enderEmit.fone);

          INIRec.WriteInteger('ide', 'cUF', ide.cUF);

          INIRec.WriteString('rem', 'CNPJCPF', Rem.CNPJCPF);
          INIRec.WriteString('rem', 'IE', Rem.IE);
          INIRec.WriteString('rem', 'xNome', Rem.xNome);
          INIRec.WriteString('rem', 'xFant', Rem.xFant);
          INIRec.WriteString('rem', 'fone', Rem.fone);

          INIRec.WriteString('rem', 'xLgr', Rem.enderReme.xLgr);
          INIRec.WriteString('rem', 'nro', Rem.enderReme.nro);
          INIRec.WriteString('rem', 'xCpl', Rem.enderReme.xCpl);
          INIRec.WriteString('rem', 'xBairro', Rem.enderReme.xBairro);
          INIRec.WriteInteger('rem', 'cMun', Rem.enderReme.cMun);
          INIRec.WriteString('rem', 'xMun', Rem.enderReme.xMun);
          INIRec.WriteInteger('rem', 'CEP', Rem.enderReme.CEP);
          INIRec.WriteString('rem', 'UF', Rem.enderReme.UF);
          INIRec.WriteInteger('rem', 'PaisCod', Rem.enderReme.cPais);
          INIRec.WriteString('rem', 'Pais', Rem.enderReme.xPais);
          INIRec.WriteString('rem', 'Email', Rem.email);

        {$IFNDEF PL_200}
          for i := 0 to Rem.infNF.Count -1 do
          begin
            sSecao := 'infNF' + IntToStrZero(I+1, 3);

            with Rem.infNF.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'nRoma', nRoma);
              INIRec.WriteString(sSecao, 'nPed', nPed);
              INIRec.WriteString(sSecao, 'mod', ModeloNFToStr(modelo));
              INIRec.WriteString(sSecao, 'serie', serie);
              INIRec.WriteString(sSecao, 'nDoc', nDoc);
              INIRec.WriteString(sSecao, 'dEmi', DateToStr(dEmi));
              INIRec.WriteString(sSecao, 'vBC', CurrToStr(vBC));
              INIRec.WriteString(sSecao, 'vICMS', CurrToStr(vICMS));
              INIRec.WriteString(sSecao, 'vBCST', CurrToStr(vBCST));
              INIRec.WriteString(sSecao, 'vST', CurrToStr(vST));
              INIRec.WriteString(sSecao, 'vProd', CurrToStr(vProd));
              INIRec.WriteString(sSecao, 'vNF', CurrToStr(vNF));
              INIRec.WriteInteger(sSecao, 'nCFOP', nCFOP);
              INIRec.WriteString(sSecao, 'nPeso', CurrToStr(nPeso));
              INIRec.WriteString(sSecao, 'PIN', PIN);
            end;
          end;

          for i := 0 to Rem.infNFe.Count -1 do
          begin
            sSecao := 'infNFe' + IntToStrZero(I+1, 3);

            with Rem.infNFe.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'chave', chave);
              INIRec.WriteString(sSecao, 'PIN', PIN);
            end;
          end;
        {$ENDIF}

          INIRec.WriteString('Dest', 'CNPJCPF', Dest.CNPJCPF);
          INIRec.WriteString('Dest', 'IE', Dest.IE);
          INIRec.WriteString('Dest', 'xNome', Dest.xNome);
          INIRec.WriteString('Dest', 'fone', Dest.fone);

          INIRec.WriteString('Dest', 'xLgr', Dest.enderDest.xLgr);
          INIRec.WriteString('Dest', 'nro', Dest.enderDest.nro);
          INIRec.WriteString('Dest', 'xCpl', Dest.enderDest.xCpl);
          INIRec.WriteString('Dest', 'xBairro', Dest.enderDest.xBairro);
          INIRec.WriteInteger('Dest', 'cMun', Dest.enderDest.cMun);
          INIRec.WriteString('Dest', 'xMun', Dest.enderDest.xMun);
          INIRec.WriteInteger('Dest', 'CEP', Dest.enderDest.CEP);
          INIRec.WriteString('Dest', 'UF', Dest.enderDest.UF);

          INIRec.WriteInteger('Dest', 'cPais', Dest.enderDest.cPais);
          INIRec.WriteString('Dest', 'xPais', Dest.enderDest.xPais);

          INIRec.WriteString('vPrest', 'vTPrest', CurrToStr(vPrest.vTPrest));
          INIRec.WriteString('vPrest', 'vRec', CurrToStr(vPrest.vRec));

          for i := 0 to vPrest.comp.Count - 1 do
          begin
            sSecao    := 'Comp' + IntToStrZero(I+1, 3);
            with vPrest.comp.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'xNome', xNome);
              INIRec.WriteString(sSecao, 'vComp', CurrToStr(vComp));
            end;
          end;

          if Imp.ICMS.ICMS00.CST <> cst00 then
          begin
            INIRec.WriteString('ICMS00', 'CST', CSTICMSToStr(Imp.ICMS.ICMS00.CST));
            INIRec.WriteString('ICMS00', 'vBC', CurrToStr(Imp.ICMS.ICMS00.vBC));
            INIRec.WriteString('ICMS00', 'pICMS', CurrToStr(Imp.ICMS.ICMS00.pICMS));
            INIRec.WriteString('ICMS00', 'vICMS', CurrToStr(Imp.ICMS.ICMS00.vICMS));
          end;

          if Imp.ICMS.ICMS20.CST <> cst00 then
          begin
            INIRec.WriteString('ICMS20', 'CST', CSTICMSToStr(Imp.ICMS.ICMS20.CST ));
            INIRec.WriteString('ICMS20', 'pRedBC', CurrToStr(Imp.ICMS.ICMS20.pRedBC));
            INIRec.WriteString('ICMS20', 'vBC', CurrToStr(Imp.ICMS.ICMS20.vBC));
            INIRec.WriteString('ICMS20', 'pICMS', CurrToStr(Imp.ICMS.ICMS20.pICMS));
            INIRec.WriteString('ICMS20', 'vICMS', CurrToStr(Imp.ICMS.ICMS20.vICMS));
          end;

          if Imp.ICMS.ICMS45.CST <> cst00 then
            INIRec.WriteString('ICMS45', 'CST', CSTICMSToStr(Imp.ICMS.ICMS45.CST));

          if Imp.ICMS.ICMS60.CST <> cst00 then
          begin
            INIRec.WriteString('ICMS60', 'CST', CSTICMSToStr(Imp.ICMS.ICMS60.CST));
            INIRec.WriteString('ICMS60', 'vBCSTRet', CurrToStr(Imp.ICMS.ICMS60.vBCSTRet));
            INIRec.WriteString('ICMS60', 'vICMSSTRet', CurrToStr(Imp.ICMS.ICMS60.vICMSSTRet));
            INIRec.WriteString('ICMS60', 'pICMSSTRet', CurrToStr(Imp.ICMS.ICMS60.pICMSSTRet));
            INIRec.WriteString('ICMS60', 'vCred', CurrToStr(Imp.ICMS.ICMS60.vCred));
          end;

          if Imp.ICMS.ICMS90.CST <> cst00 then
          begin
            INIRec.WriteString('ICMS90', 'CST', CSTICMSToStr(Imp.ICMS.ICMS90.CST));
            INIRec.WriteString('ICMS90', 'pRedBC', CurrToStr(Imp.ICMS.ICMS90.pRedBC));
            INIRec.WriteString('ICMS90', 'vBC', CurrToStr(Imp.ICMS.ICMS90.vBC));
            INIRec.WriteString('ICMS90', 'pICMS', CurrToStr(Imp.ICMS.ICMS90.pICMS));
            INIRec.WriteString('ICMS90', 'vICMS', CurrToStr(Imp.ICMS.ICMS90.vICMS));
            INIRec.WriteString('ICMS90', 'vCred', CurrToStr(Imp.ICMS.ICMS90.vCred));
          end;

          if Imp.ICMS.ICMSOutraUF.CST <> cst00 then
          begin
            INIRec.WriteString('ICMSOutraUF', 'CST', CSTICMSToStr(Imp.ICMS.ICMSOutraUF.CST));
            INIRec.WriteString('ICMSOutraUF', 'pRedBCOutraUF', CurrToStr(Imp.ICMS.ICMSOutraUF.pRedBCOutraUF));
            INIRec.WriteString('ICMSOutraUF', 'vBCOutraUF', CurrToStr(Imp.ICMS.ICMSOutraUF.vBCOutraUF));
            INIRec.WriteString('ICMSOutraUF', 'pICMSOutraUF', CurrToStr(Imp.ICMS.ICMSOutraUF.pICMSOutraUF));
            INIRec.WriteString('ICMSOutraUF', 'vICMSOutraUF', CurrToStr(Imp.ICMS.ICMSOutraUF.vICMSOutraUF));
          end;

          {indica se é simples}
          if (Imp.ICMS.ICMSSN.indSN = 1) and (Imp.ICMS.SituTrib = cstICMSSN) then
            INIRec.WriteInteger('ICMSSN', 'indSN', Imp.ICMS.ICMSSN.indSN);

          INIRec.WriteFloat('ICMSUFFim', 'vBCUFFim', Imp.ICMSUFFim.vBCUFFim);
          INIRec.WriteFloat('ICMSUFFim', 'pFCPUFFim', Imp.ICMSUFFim.pFCPUFFim);
          INIRec.WriteFloat('ICMSUFFim', 'pICMSUFFim', Imp.ICMSUFFim.pICMSUFFim);
          INIRec.WriteFloat('ICMSUFFim', 'pICMSInter', Imp.ICMSUFFim.pICMSInter);
          INIRec.WriteFloat('ICMSUFFim', 'pICMSInterPart', Imp.ICMSUFFim.pICMSInterPart);
          INIRec.WriteFloat('ICMSUFFim', 'vFCPUFFim', Imp.ICMSUFFim.vFCPUFFim);
          INIRec.WriteFloat('ICMSUFFim', 'vICMSUFFim', Imp.ICMSUFFim.vICMSUFFim);
          INIRec.WriteFloat('ICMSUFFim', 'vICMSUFIni', Imp.ICMSUFFim.vICMSUFIni);

        {$IFDEF PL_200}
          INIRec.WriteString('infCarga', 'vCarga', CurrToStr(infCTeNorm.infCarga.vCarga));
          INIRec.WriteString('infCarga', 'proPred', infCTeNorm.infCarga.proPred);
          INIRec.WriteString('infCarga', 'xOutCat', infCTeNorm.infCarga.xOutCat);
        {$ELSE}
          INIRec.WriteString('infCarga', 'vCarga', CurrToStr(infCarga.vCarga));
          INIRec.WriteString('infCarga', 'proPred', infCarga.proPred);
          INIRec.WriteString('infCarga', 'xOutCat', infCarga.xOutCat);
        {$ENDIF}

        {$IFDEF PL_200}
          for i := 0 to infCTeNorm.infCarga.infQ.Count -1 do
        {$ELSE}
          for i := 0 to infCarga.infQ.Count -1 do
        {$ENDIF}
          begin
            sSecao := 'infQ' + IntToStrZero(I+1, 3);

        {$IFDEF PL_200}
            with infCTeNorm.infCarga.infQ.Items[i] do
        {$ELSE}
            with infCarga.infQ.Items[i] do
        {$ENDIF}
            begin
              INIRec.WriteString(sSecao, 'cUnid', UnidMedToStr(cUnid));
              INIRec.WriteString(sSecao, 'tpMed', tpMed);
              INIRec.WriteString(sSecao, 'qCarga', CurrToStr(qCarga));
            end;
          end;

        {$IFDEF PL_200}
          for i := 0 to infCTeNorm.infDoc.infNF.Count -1 do
          begin
            sSecao := 'infNF' + IntToStrZero(I+1, 3);

            with infCTeNorm.infDoc.infNF.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'nRoma', nRoma);
              INIRec.WriteString(sSecao, 'nPed', nPed);
              INIRec.WriteString(sSecao, 'mod', ModeloNFToStr(modelo));
              INIRec.WriteString(sSecao, 'serie', serie);
              INIRec.WriteString(sSecao, 'nDoc', nDoc);
              INIRec.WriteString(sSecao, 'dEmi', DateToStr(dEmi));
              INIRec.WriteString(sSecao, 'vBC', CurrToStr(vBC));
              INIRec.WriteString(sSecao, 'vICMS', CurrToStr(vICMS));
              INIRec.WriteString(sSecao, 'vBCST', CurrToStr(vBCST));
              INIRec.WriteString(sSecao, 'vST', CurrToStr(vST));
              INIRec.WriteString(sSecao, 'vProd', CurrToStr(vProd));
              INIRec.WriteString(sSecao, 'vNF', CurrToStr(vNF));
              INIRec.WriteInteger(sSecao, 'nCFOP', nCFOP);
              INIRec.WriteString(sSecao, 'nPeso', CurrToStr(nPeso));
              INIRec.WriteString(sSecao, 'PIN', PIN);
            end;
          end;

          for i := 0 to infCTeNorm.infDoc.infNFe.Count -1 do
          begin
            sSecao := 'infNFe' + IntToStrZero(I+1, 3);

            with infCTeNorm.infDoc.infNFe.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'chave', chave);
              INIRec.WriteString(sSecao, 'PIN', PIN);
            end;
          end;
        {$ENDIF}

        {$IFDEF PL_200}
          for i:= 0 to infCTeNorm.seg.Count - 1 do
        {$ELSE}
          for i:= 0 to infSeg.Count - 1 do
        {$ENDIF}
          begin
            sSecao := 'infSeg' + IntToStrZero(I+1, 3);

        {$IFDEF PL_200}
            with infCTeNorm.seg.Items[i] do
        {$ELSE}
            with infSeg.Items[i] do
        {$ENDIF}
            begin
              INIRec.WriteString(sSecao, 'respSeg', TpRspSeguroToStr(respSeg));
              INIRec.WriteString(sSecao, 'xSeg', xSeg);
              INIRec.WriteString(sSecao, 'nApol', nApol);
              INIRec.WriteString(sSecao, 'nAver', nAver);
              INIRec.WriteString(sSecao, 'vCarga', CurrToStr(vCarga));
            end;
          end;

        {$IFDEF PL_200}
          if infCTeNorm.Rodo.RNTRC <> '' then
          begin
            INIRec.WriteString('Rodo', 'RNTRC', infCTeNorm.Rodo.RNTRC);
            INIRec.WriteString('Rodo', 'dPrev', DateToStr(infCTeNorm.Rodo.dPrev));
            INIRec.WriteString('Rodo', 'lota', TpLotacaoToStr(infCTeNorm.Rodo.Lota));
          end;
        {$ELSE}
          if Rodo.RNTRC <> '' then
          begin
            INIRec.WriteString('Rodo', 'RNTRC', Rodo.RNTRC);
            INIRec.WriteString('Rodo', 'dPrev', DateToStr(Rodo.dPrev));
            INIRec.WriteString('Rodo', 'lota', TpLotacaoToStr(Rodo.Lota));
          end;
        {$ENDIF}

        {$IFDEF PL_200}
          for i := 0 to autXML.Count - 1 do
          begin
            sSecao := 'autXML' + IntToStrZero(I+1, 2);

            with autXML.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
            end;
          end;
        {$ENDIF}
       end;
   finally
      IniCTe := TStringList.Create;
      INIRec.GetStrings(IniCTe);
      INIRec.Free;
      Result := StringReplace(IniCTe.Text,sLineBreak+sLineBreak,sLineBreak,[rfReplaceAll]);
      IniCTe.Free;
   end;

  end;

end;

procedure GerarCTeIniEvento( AStr: String );
var
  I, J   : Integer;
  sSecao, sFim : String;
  INIRec : TMemIniFile;
  ok     : Boolean;
begin
  INIRec := LerConverterIni(AStr);

  with FrmACBrMonitor do
  begin
   try
     ACBrCTe1.EventoCTe.idLote := INIRec.ReadInteger('EVENTO', 'idLote', 0);
     ACBrCTe1.EventoCTe.Evento.Clear;
     I := 1;
     while true do
      begin
        sSecao := 'EVENTO'+IntToStrZero(I,3);
        sFim   := INIRec.ReadString(sSecao, 'chCTe', 'FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
           break;

        with ACBrCTe1.EventoCTe.Evento.Add do
         begin
           infEvento.chCTe              := INIRec.ReadString(sSecao, 'chCTe', '');
           infEvento.cOrgao             := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
           infEvento.CNPJ               := INIRec.ReadString(sSecao, 'CNPJ', '');
           infEvento.dhEvento           := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
           infEvento.tpEvento           := StrToTpEvento(ok,INIRec.ReadString(sSecao, 'tpEvento', ''));
           infEvento.nSeqEvento         := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);
           infEvento.detEvento.xCondUso := '';
           infEvento.detEvento.xJust    := INIRec.ReadString(sSecao, 'xJust', '');
           infEvento.detEvento.nProt    := INIRec.ReadString(sSecao, 'nProt', '');

           ACBrCTe1.EventoCTe.Evento.Items[I-1].InfEvento.detEvento.infCorrecao.Clear;

           J := 1;
           while true do
            begin
              sSecao := 'DETEVENTO' + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'grupoAlterado', 'FIM');
              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with ACBrCTe1.EventoCTe.Evento.Items[I-1].InfEvento.detEvento.infCorrecao.Add do
               begin
                 grupoAlterado   := INIRec.ReadString(sSecao, 'grupoAlterado', '');
                 campoAlterado   := INIRec.ReadString(sSecao, 'campoAlterado', '');
                 valorAlterado   := INIRec.ReadString(sSecao, 'valorAlterado', '');
                 nroItemAlterado := INIRec.ReadInteger(sSecao, 'nroItemAlterado', 0);
               end;
              Inc(J);
            end;
         end;
        Inc(I);
      end;
   finally
      INIRec.Free;
   end;
 end;
end;

end.

