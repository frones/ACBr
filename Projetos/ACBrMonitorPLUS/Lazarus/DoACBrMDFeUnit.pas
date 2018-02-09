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

unit DoACBrMDFeUnit;

interface
Uses Classes, SysUtils, CmdUnit,
     ACBrUtil, ACBrDFeUtil;

Procedure DoACBrMDFe( Cmd : TACBrCmd );
procedure GerarIniMDFe( AStr: String );
function GerarMDFeIni( XML : String ) : String;

implementation

Uses IniFiles, DateUtils, Forms, strutils,
  ACBrMonitor1, ACBrDFeConfiguracoes,
  pcnConversao, pmdfeConversaoMDFe, DoACBrNFeUnit,
  pcnAuxiliar, pmdfeMDFeR, DoACBrUnit, pmdfeMDFe;

Procedure DoACBrMDFe( Cmd : TACBrCmd );
var
  I,J : Integer;
  ArqMDFe, ArqPDF, Chave : String;
  Salva,  OK : Boolean;
  SL     : TStringList;
  Alertas : AnsiString;

  sMensagemEmail: TStringList;
  CC, Anexos: Tstrings;

  Memo   , PathsMDFe: TStringList;
  Files  , ArqEventoMDFe: String;
  dtFim  : TDateTime;

  RetFind   : Integer;
  SearchRec : TSearchRec;
  FormaEmissao: TpcnTipoEmissao;
  VersaoDF: TVersaoMDFe;

//  MDFeRTXT            :  TNFeRTXT;
begin
 with FrmACBrMonitor do
  begin
     try
        if Cmd.Metodo = 'statusservico' then
         begin
           if ACBrMDFe1.WebServices.StatusServico.Executar then
            begin

              Cmd.Resposta := ACBrMDFe1.WebServices.StatusServico.Msg+
                              '[STATUS]'+sLineBreak+
                              'Versao='+ACBrMDFe1.WebServices.StatusServico.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.StatusServico.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrMDFe1.WebServices.StatusServico.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrMDFe1.WebServices.StatusServico.CStat)+sLineBreak+
                              'XMotivo='+ACBrMDFe1.WebServices.StatusServico.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrMDFe1.WebServices.StatusServico.CUF)+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrMDFe1.WebServices.StatusServico.DhRecbto)+sLineBreak+
                              'TMed='+IntToStr(ACBrMDFe1.WebServices.StatusServico.TMed)+sLineBreak+
                              'DhRetorno='+DateTimeToStr(ACBrMDFe1.WebServices.StatusServico.DhRetorno)+sLineBreak+
                              'XObs='+ACBrMDFe1.WebServices.StatusServico.XObs+sLineBreak;
            end;
         end
        else if Cmd.Metodo = 'validarmdfe' then
         begin
           ACBrMDFe1.Manifestos.Clear;
           CarregarDFe(Cmd.Params(0), ArqMDFe, tDFeMDFe);
           ACBrMDFe1.Manifestos.Validar;
         end
        else if Cmd.Metodo = 'assinarmdfe' then
         begin
           ACBrMDFe1.Manifestos.Clear;
           CarregarDFe(Cmd.Params(0), ArqMDFe, tDFeMDFe);
           Salva := ACBrMDFe1.Configuracoes.Geral.Salvar;

           if not Salva then
           begin
             ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
             ACBrMDFe1.Configuracoes.Arquivos.PathSalvar := PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs';
           end;

           ACBrMDFe1.Configuracoes.Geral.Salvar := True;
           ACBrMDFe1.Manifestos.Assinar;
           ACBrMDFe1.Configuracoes.Geral.Salvar := Salva;

           if NaoEstaVazio(ACBrMDFe1.Manifestos.Items[0].NomeArq) then
             Cmd.Resposta := ACBrMDFe1.Manifestos.Items[0].NomeArq
           else
             Cmd.Resposta := PathWithDelim(ACBrMDFe1.Configuracoes.Arquivos.PathSalvar)+StringReplace(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID, 'MDFe', '', [rfIgnoreCase])+'-mdfe.xml';
         end
        else if Cmd.Metodo = 'consultarmdfe' then
         begin
           ACBrMDFe1.Manifestos.Clear;
		   
           PathsMDFe := TStringList.Create;
           try
             PathsMDFe.Append(Cmd.Params(0));
             PathsMDFe.Append(PathWithDelim(ACBrMDFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             try
               CarregarDFe(PathsMDFe, ArqMDFe, tDFeMDFe);
             except
             end;
           finally
             PathsMDFe.Free;
           end;

           if ACBrMDFe1.Manifestos.Count = 0 then
           begin
             if ValidarChave(Cmd.Params(0)) then
               ACBrMDFe1.WebServices.Consulta.MDFeChave := Cmd.Params(0)
             else
               raise Exception.Create('Parâmetro inválido. Chave do MDFe inválida ou arquivo não encontrado.');
           end
           else
             ACBrMDFe1.WebServices.Consulta.MDFeChave := OnlyNumber(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID);

           try
             ACBrMDFe1.WebServices.Consulta.Executar;

             Cmd.Resposta := ACBrMDFe1.WebServices.Consulta.Msg+sLineBreak+
                              '[CONSULTA]'+sLineBreak+
                              'Versao='+ACBrMDFe1.WebServices.Consulta.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.Consulta.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrMDFe1.WebServices.Consulta.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrMDFe1.WebServices.Consulta.CStat)+sLineBreak+
                              'XMotivo='+ACBrMDFe1.WebServices.Consulta.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrMDFe1.WebServices.Consulta.CUF)+sLineBreak+
                              'ChMDFe='+ACBrMDFe1.WebServices.Consulta.MDFeChave+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrMDFe1.WebServices.Consulta.DhRecbto)+sLineBreak+
                              'NProt='+ACBrMDFe1.WebServices.Consulta.Protocolo+sLineBreak+
                              'DigVal='+ACBrMDFe1.WebServices.Consulta.protMDFe.digVal+sLineBreak;

           except
             raise Exception.Create(ACBrMDFe1.WebServices.Consulta.Msg);
           end;
         end
        else if Cmd.Metodo = 'cancelarmdfe' then
         begin
           if not ValidarChave(Cmd.Params(0)) then
             raise Exception.Create('Chave '+Cmd.Params(0)+' inválida.')
           else
             ACBrMDFe1.WebServices.Consulta.MDFeChave := Cmd.Params(0);

           if not ACBrMDFe1.WebServices.Consulta.Executar then
              raise Exception.Create(ACBrMDFe1.WebServices.Consulta.Msg);

           ACBrMDFe1.EventoMDFe.Evento.Clear;
           with ACBrMDFe1.EventoMDFe.Evento.Add do
           begin
             infEvento.CNPJ := Cmd.Params(2);
             if Trim(infEvento.CNPJ) = '' then
                infEvento.CNPJ := copy(OnlyNumber(ACBrMDFe1.WebServices.Consulta.MDFeChave),7,14)
             else
             begin
                if not ValidarCNPJ(Cmd.Params(2)) then
                  raise Exception.Create('CNPJ '+Cmd.Params(2)+' inválido.')
             end;

             infEvento.cOrgao   := StrToIntDef(copy(OnlyNumber(ACBrMDFe1.WebServices.Consulta.MDFeChave),1,2),0);
             infEvento.dhEvento := now;
             infEvento.tpEvento := teCancelamento;
             infEvento.chMDFe   := ACBrMDFe1.WebServices.Consulta.MDFeChave;
             infEvento.detEvento.nProt := ACBrMDFe1.WebServices.Consulta.Protocolo;
             infEvento.detEvento.xJust := Cmd.Params(1);
           end;
           try
              ACBrMDFe1.EnviarEvento(StrToIntDef(Cmd.Params(3),1));

              Cmd.Resposta := ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.xMotivo+sLineBreak+
                              '[CANCELAMENTO]'+sLineBreak+
                              'Versao='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat)+sLineBreak+
                              'XMotivo='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cOrgao)+sLineBreak+
                              'ChMDFe='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.chMDFe+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento)+sLineBreak+
                              'NProt='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt+sLineBreak+
                              'tpEvento='+TpEventoToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.tpEvento)+sLineBreak+
                              'xEvento='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xEvento+sLineBreak+
                              'nSeqEvento='+IntToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nSeqEvento)+sLineBreak+
                              'CNPJDest='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.CNPJDest+sLineBreak+
                              'emailDest='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.emailDest+sLineBreak+
                              'XML='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.XML+sLineBreak;
           except
              raise Exception.Create(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.xMotivo);
           end;
         end
        else if Cmd.Metodo = 'encerrarmdfe' then
         begin
           ACBrMDFe1.Manifestos.Clear;

           PathsMDFe := TStringList.Create;
           try
             PathsMDFe.Append(Cmd.Params(0));
             PathsMDFe.Append(PathWithDelim(ACBrMDFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             try
               CarregarDFe(PathsMDFe, ArqMDFe, tDFeMDFe);
             except
                raise Exception.Create('Erro ao Carregar MDFe em: '+PathsMDFe.Text);
             end;
           finally
             PathsMDFe.Free;
           end;

           if (ACBrMDFe1.Manifestos.Count = 0) then
           begin
             if ValidarChave(Cmd.Params(0)) then
               Chave := Cmd.Params(0)
             else
               raise Exception.Create('Parâmetro inválido. Chave do MDFe inválida ou arquivo não encontrado.');
           end
           else
             Chave := OnlyNumber(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID);

           ACBrMDFe1.EventoMDFe.Evento.Clear;
           with ACBrMDFe1.EventoMDFe.Evento.Add do
           begin
             infEvento.CNPJ := Cmd.Params(3);
             if Trim(infEvento.CNPJ) = '' then
                infEvento.CNPJ := copy(chave,7,14)
             else
             begin
                if not ValidarCNPJ(Cmd.Params(3)) then
                  raise Exception.Create('CNPJ '+Cmd.Params(3)+' inválido.')
             end;

             infEvento.cOrgao   := StrToIntDef(copy(OnlyNumber(chave),1,2),0);
             infEvento.dhEvento := now;
             infEvento.tpEvento := teEncerramento;
             infEvento.chMDFe   := Chave;

             if (Trim(Cmd.Params(4)) <> '') then
               infEvento.detEvento.nProt := Trim(Cmd.Params(4))
             else if ((ACBrMDFe1.Manifestos.Count > 0)
                       and (ACBrMDFe1.Manifestos.Items[0].MDFe.procMDFe.nProt <> '')) then
               infEvento.detEvento.nProt := ACBrMDFe1.Manifestos.Items[0].MDFe.procMDFe.nProt
             else
             begin
               //Realiza Consulta na Sefaz
               ACBrMDFe1.WebServices.Consulta.MDFeChave := Chave;
               if not (ACBrMDFe1.WebServices.Consulta.Executar) then
                 raise Exception.Create('Parâmetro inválido. ' + ACBrMDFe1.WebServices.Consulta.Msg)
               else
                 infEvento.detEvento.nProt := ACBrMDFe1.WebServices.Consulta.Protocolo;
             end;

             if (Trim(Cmd.Params(2)) <> '') then
             begin
               infEvento.detEvento.cUF   := StrToIntDef(copy(Cmd.Params(2), 1, 2), 1);
               infEvento.detEvento.cMun  := StrToIntDef(Cmd.Params(2), 1);
             end
             else if ((ACBrMDFe1.Manifestos.Count > 0)
                  and (ACBrMDFe1.Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga > 0 )) then
             begin
               infEvento.detEvento.cMun := ACBrMDFe1.Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga;
               infEvento.detEvento.cUF  := StrToIntDef(copy(intToStr(ACBrMDFe1.Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga), 1, 2), 1);
             end;

             infEvento.detEvento.dtEnc := StringToDateTime(Cmd.Params(1));
           end;
           try
              ACBrMDFe1.EnviarEvento(StrToIntDef(Cmd.Params(5),1));

              Cmd.Resposta := ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.xMotivo+sLineBreak+
                              '[ENCERRAMENTO]'+sLineBreak+
                              'Versao='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat)+sLineBreak+
                              'XMotivo='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cOrgao)+sLineBreak+
                              'ChMDFe='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.chMDFe+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento)+sLineBreak+
                              'NProt='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt+sLineBreak+
                              'tpEvento='+TpEventoToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.tpEvento)+sLineBreak+
                              'xEvento='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xEvento+sLineBreak+
                              'nSeqEvento='+IntToStr(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nSeqEvento)+sLineBreak+
                              'CNPJDest='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.CNPJDest+sLineBreak+
                              'emailDest='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.emailDest+sLineBreak+
                              'XML='+ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.XML+sLineBreak;
           except
              raise Exception.Create(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.xMotivo);
           end;
         end

        else if Cmd.Metodo = 'consultamdfenaoenc' then
        begin
          if not ValidarCNPJ(Cmd.Params(0)) then
            raise Exception.Create('CNPJ '+Cmd.Params(0)+' invalido.');
          try
            ACBrMDFe1.WebServices.ConsultaMDFeNaoEnc( Cmd.Params(0) );
            Cmd.Resposta := ACBrMDFe1.WebServices.ConsMDFeNaoEnc.Msg+sLineBreak+
                           '[NAOENCERRADOS]'+sLineBreak+
                           'Versao='+ACBrMDFe1.WebServices.ConsMDFeNaoEnc.verAplic+sLineBreak+
                           'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.ConsMDFeNaoEnc.TpAmb)+sLineBreak+
                           'VerAplic='+ACBrMDFe1.WebServices.ConsMDFeNaoEnc.VerAplic+sLineBreak+
                           'CUF='+IntToStr(ACBrMDFe1.WebServices.ConsMDFeNaoEnc.CUF)+sLineBreak+
                           'CNPJ='+ACBrMDFe1.WebServices.ConsMDFeNaoEnc.CNPJ+sLineBreak+
                           'CStat='+IntToStr(ACBrMDFe1.WebServices.ConsMDFeNaoEnc.CStat)+sLineBreak+
                           'XMotivo='+ACBrMDFe1.WebServices.ConsMDFeNaoEnc.XMotivo+sLineBreak+
                           'CUF='+IntToStr(ACBrMDFe1.WebServices.ConsMDFeNaoEnc.CUF)+sLineBreak+
                           'ChMDFe='+ACBrMDFe1.WebServices.ConsMDFeNaoEnc.InfMDFe.Items[0].chMDFe+sLineBreak+
                           'NProt='+ACBrMDFe1.WebServices.ConsMDFeNaoEnc.InfMDFe.Items[0].nProt+sLineBreak;
          except
            raise Exception.Create(ACBrMDFe1.WebServices.ConsMDFeNaoEnc.Msg);
          end;
        end

        else if Cmd.Metodo = 'imprimirdamdfe' then
         begin
           ACBrMDFe1.Manifestos.Clear;
           PathsMDFe := TStringList.Create;
             try
               PathsMDFe.Append(Cmd.Params(0));
               PathsMDFe.Append(PathWithDelim(ACBrMDFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
               try
                 CarregarDFe(PathsMDFe, ArqMDFe, tDFeMDFe);
               except
               end;
             finally
               PathsMDFe.Free;
             end;

           if NaoEstaVazio(Cmd.Params(1)) then
             ACBrMDFe1.DAMDFe.Impressora := Cmd.Params(1)
           else
             ACBrMDFe1.DAMDFe.Impressora := cbxImpressora.Text;

           if NaoEstaVazio(Cmd.Params(2)) then
             ACBrMDFe1.DAMDFe.NumCopias := StrToIntDef(Cmd.Params(2),1)
           else
             ACBrMDFe1.DAMDFe.NumCopias := edtNumCopia.Value;

           if NaoEstaVazio(Cmd.Params(3)) then
             ACBrMDFe1.DAMDFe.ProtocoloMDFe := Cmd.Params(3);

           try
             AntesDeImprimir(ACBrMDFe1.DAMDFe.MostrarPreview);
             ACBrMDFe1.Manifestos.Imprimir;
           finally
             DepoisDeImprimir;
           end;

           Cmd.Resposta := 'DAMDFe Impresso com sucesso';
         end
        else if Cmd.Metodo = 'imprimirdamdfepdf' then
         begin
           ACBrMDFe1.Manifestos.Clear;
           CarregarDFe(Cmd.Params(0), ArqMDFe, tDFeMDFe);

           if NaoEstaVazio(Cmd.Params(1)) then
              ACBrMDFe1.DAMDFe.ProtocoloMDFe := Cmd.Params(1);

           try
              ACBrMDFe1.Manifestos.ImprimirPDF;
              ArqPDF := OnlyNumber(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID)+'-mdfe.pdf';
              Cmd.Resposta := 'Arquivo criado em: '+ PathWithDelim(ACBrMDFe1.DAMDFe.PathPDF) +
                              ArqPDF;
           except
              raise Exception.Create('Erro ao criar o arquivo PDF');
           end;
         end
        else if ( Cmd.Metodo = 'imprimirevento') or ( Cmd.Metodo = 'imprimireventopdf' ) then
         begin
           ACBrMDFe1.EventoMDFe.Evento.Clear;
           PathsMDFe := TStringList.Create;
           try
             PathsMDFe.Append(Cmd.Params(0));
             PathsMDFe.Append(PathWithDelim(ACBrMDFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             try
               CarregarDFe(PathsMDFe, ArqEventoMDFe, tDFeEventoMDFe);
             except
             end;
           finally
             PathsMDFe.Free;
           end;

           ACBrMDFe1.Manifestos.Clear;
           if NaoEstaVazio(Cmd.Params(1)) then
           begin
             PathsMDFe := TStringList.Create;
             try
               PathsMDFe.Append(Cmd.Params(1));
               PathsMDFe.Append(PathWithDelim(ACBrMDFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
               try
                 CarregarDFe(PathsMDFe, ArqMDFe, tDFeMDFe);
               except
               end;
             finally
               PathsMDFe.Free;
             end;
           end;

           if Cmd.Metodo = 'imprimireventopdf' then
           begin
             try
                ACBrMDFe1.ImprimirEventoPDF;
                ArqPDF := OnlyNumber(ACBrMDFe1.EventoMDFe.Evento[0].InfEvento.Id);
                ArqPDF := PathWithDelim(ACBrMDFe1.DAMDFe.PathPDF)+ArqPDF+'-procEventoMDFe.pdf';
                Cmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
             except
                raise Exception.Create('Erro ao criar o arquivo PDF');
             end;
           end
           else
           begin
             if NaoEstaVazio(Cmd.Params(2)) then
                ACBrMDFe1.DAMDFe.Impressora := Cmd.Params(2)
             else
                ACBrMDFe1.DAMDFe.Impressora := cbxImpressora.Text;

             if NaoEstaVazio(Cmd.Params(3)) then
                ACBrMDFe1.DAMDFe.NumCopias := StrToIntDef(Cmd.Params(3),1)
             else
                ACBrMDFe1.DAMDFe.NumCopias := edtNumCopia.Value;

             try
               AntesDeImprimir(ACBrMDFe1.DAMDFE.MostrarPreview);
               ACBrMDFe1.ImprimirEvento;
             finally
               DepoisDeImprimir;
             end;

             Cmd.Resposta := 'Evento Impresso com sucesso';
           end;
         end

        else if Cmd.Metodo = 'inutilizarmdfe' then
         begin                            //CNPJ         //Justificat   //Ano                    //Modelo                 //Série                  //Num.Inicial            //Num.Final
           raise Exception.Create('Método: MDFe.InutilizarMDFe não implementado.');
         end

        else if Cmd.Metodo = 'enviarmdfe' then
         begin
           ACBrMDFe1.Manifestos.Clear;
           CarregarDFe(Cmd.Params(0), ArqMDFe, tDFeMDFe);
           ACBrMDFe1.Manifestos.GerarMDFe;

           if Cmd.Params(2) <> '0' then
              ACBrMDFe1.Manifestos.Assinar;

           ACBrMDFe1.Manifestos.Validar;

           if not(ACBrMDFe1.WebServices.StatusServico.Executar) then
             raise Exception.Create(ACBrMDFe1.WebServices.StatusServico.Msg);

           if Trim(OnlyNumber(Cmd.Params(1))) = '' then
             ACBrMDFe1.WebServices.Enviar.Lote := '1'
           else
             ACBrMDFe1.WebServices.Enviar.Lote := OnlyNumber(Cmd.Params(1)); //StrToIntDef( OnlyNumber(Cmd.Params(1)),1);

           ACBrMDFe1.WebServices.Enviar.Executar;

           Cmd.Resposta :=  ACBrMDFe1.WebServices.Enviar.Msg+sLineBreak+
                            '[ENVIO]'+sLineBreak+
                            'Versao='+ACBrMDFe1.WebServices.Enviar.verAplic+sLineBreak+
                            'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.Enviar.TpAmb)+sLineBreak+
                            'VerAplic='+ACBrMDFe1.WebServices.Enviar.VerAplic+sLineBreak+
                            'CStat='+IntToStr(ACBrMDFe1.WebServices.Enviar.CStat)+sLineBreak+
                            'XMotivo='+ACBrMDFe1.WebServices.Enviar.XMotivo+sLineBreak+
                            'CUF='+IntToStr(ACBrMDFe1.WebServices.Enviar.CUF)+sLineBreak+
                            'NRec='+ACBrMDFe1.WebServices.Enviar.Recibo+sLineBreak+
                            'DhRecbto='+DateTimeToStr( ACBrMDFe1.WebServices.Enviar.dhRecbto)+sLineBreak+
                            'TMed='+IntToStr( ACBrMDFe1.WebServices.Enviar.tMed)+sLineBreak;

           ACBrMDFe1.WebServices.Retorno.Recibo := ACBrMDFe1.WebServices.Enviar.Recibo;
           ACBrMDFe1.WebServices.Retorno.Executar;

           Cmd.Resposta :=  Cmd.Resposta+
                            ACBrMDFe1.WebServices.Retorno.Msg+sLineBreak+
                            '[RETORNO]'+sLineBreak+
                            'Versao='+ACBrMDFe1.WebServices.Retorno.verAplic+sLineBreak+
                            'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.Retorno.TpAmb)+sLineBreak+
                            'VerAplic='+ACBrMDFe1.WebServices.Retorno.VerAplic+sLineBreak+
                            'NRec='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.nRec+sLineBreak+
                            'CStat='+IntToStr(ACBrMDFe1.WebServices.Retorno.CStat)+sLineBreak+
                            'XMotivo='+ACBrMDFe1.WebServices.Retorno.XMotivo+sLineBreak+
                            'CUF='+IntToStr(ACBrMDFe1.WebServices.Retorno.CUF)+sLineBreak;

           for I:= 0 to ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Count-1 do
            begin
              for J:= 0 to ACBrMDFe1.Manifestos.Count-1 do
              begin
                if 'MDFe'+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].chMDFe = ACBrMDFe1.Manifestos.Items[j].MDFe.infMDFe.Id  then
                begin
                  Cmd.Resposta := Cmd.Resposta+
                             '[MDFe'+Trim(IntToStr(ACBrMDFe1.Manifestos.Items[J].MDFe.Ide.nMDF))+']'+sLineBreak+
                             'Versao='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].verAplic+sLineBreak+
                             'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].tpAmb)+sLineBreak+
                             'VerAplic='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].verAplic+sLineBreak+
                             'CStat='+IntToStr(ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].cStat)+sLineBreak+
                             'XMotivo='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].xMotivo+sLineBreak+
                             'CUF='+IntToStr(ACBrMDFe1.WebServices.Retorno.MDFeRetorno.cUF)+sLineBreak+
                             'ChMDFe='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].chMDFe+sLineBreak+
                             'DhRecbto='+DateTimeToStr(ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].dhRecbto)+sLineBreak+
                             'NProt='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].nProt+sLineBreak+
                             'DigVal='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].digVal+sLineBreak;
                  break;
                end;
              end;

              if NaoEstaVazio(Cmd.Params(4)) then
                 ACBrMDFe1.DAMDFe.Impressora := Cmd.Params(4)
              else
                 ACBrMDFe1.DAMDFe.Impressora := cbxImpressora.Text;

              if ACBrMDFe1.Manifestos.Items[i].Confirmado and (Cmd.Params(3) = '1') then
              begin
                try
                  AntesDeImprimir(ACBrMDFe1.DAMDFe.MostrarPreview);
                  ACBrMDFe1.Manifestos.Items[i].Imprimir;
                finally
                  DepoisDeImprimir;
                end;
              end;
            end;
         end
        else if (Cmd.Metodo = 'recibomdfe')then
         begin
           ACBrMDFe1.WebServices.Recibo.Recibo := Cmd.Params(0);
           if not(ACBrMDFe1.WebServices.Recibo.Executar) then
             raise Exception.Create(ACBrMDFe1.WebServices.Recibo.xMotivo);

           Cmd.Resposta :=  Cmd.Resposta+
                            ACBrMDFe1.WebServices.Recibo.Msg+sLineBreak+
                           '[RETORNO]'+sLineBreak+
                           'Versao='+ACBrMDFe1.WebServices.Recibo.verAplic+sLineBreak+
                           'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.Recibo.TpAmb)+sLineBreak+
                           'VerAplic='+ACBrMDFe1.WebServices.Recibo.VerAplic+sLineBreak+
                           'NRec='+ACBrMDFe1.WebServices.Recibo.Recibo+sLineBreak+
                           'CStat='+IntToStr(ACBrMDFe1.WebServices.Recibo.CStat)+sLineBreak+
                           'XMotivo='+ACBrMDFe1.WebServices.Recibo.XMotivo+sLineBreak+
                           'CUF='+IntToStr(ACBrMDFe1.WebServices.Recibo.CUF)+sLineBreak+
                           'ChMDFe='+ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[0].chMDFe+sLineBreak+
                           'NProt='+ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[0].nProt+sLineBreak+
                           'MotivoMDFe='+ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[0].xMotivo+sLineBreak;

                           for I:= 0 to ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Count-1 do
                            begin
                              Cmd.Resposta := Cmd.Resposta+
                                '[MDFe'+Trim(IntToStr(StrToInt(copy(ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].chMDFe,26,9))))+']'+sLineBreak+
                                'Versao='+ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].verAplic+sLineBreak+
                                'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].tpAmb)+sLineBreak+
                                'VerAplic='+ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].verAplic+sLineBreak+
                                'CStat='+IntToStr(ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].cStat)+sLineBreak+
                                'XMotivo='+ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].xMotivo+sLineBreak+
                                'CUF='+IntToStr(ACBrMDFe1.WebServices.Recibo.MDFeRetorno.cUF)+sLineBreak+
                                'ChMDFe='+ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].chMDFe+sLineBreak+
                                'DhRecbto='+DateTimeToStr(ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].dhRecbto)+sLineBreak+
                                'NProt='+ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].nProt+sLineBreak+
                                'DigVal='+ACBrMDFe1.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[i].digVal+sLineBreak;
                            end;

           if ACBrMDFe1.Configuracoes.Geral.Salvar then
            begin
              Cmd.Resposta :=  Cmd.Resposta+
              'Arquivo='+ACBrMDFe1.Configuracoes.Arquivos.PathSalvar+Cmd.Params(0)+'-pro-rec.xml';
            end;
         end
        else if (Cmd.Metodo = 'consultacadastro')then
         begin
           raise Exception.Create('Método: MDFe.ConsultaCadastro não implementado.');
         end
        else if (Cmd.Metodo = 'criarmdfe')      or (Cmd.Metodo = 'criarenviarmdfe') or
                (Cmd.Metodo = 'criarmdfesefaz') or (Cmd.Metodo = 'criarenviarmdfesefaz') or
                (Cmd.Metodo = 'adicionarmdfe')  or (Cmd.Metodo = 'adicionarmdfesefaz') or
                (Cmd.Metodo = 'enviarlotemdfe') then
         begin
           if (Cmd.Metodo = 'criarmdfe') or (Cmd.Metodo = 'criarenviarmdfe') or
              (Cmd.Metodo = 'adicionarmdfe') then
              GerarIniMDFe( Cmd.Params(0)  );

           if (Cmd.Metodo = 'adicionarmdfe')  or (Cmd.Metodo = 'adicionarmdfesefaz') then
            begin
              ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(1)));
              ACBrMDFe1.Manifestos.GerarMDFe;
              Alertas := ACBrMDFe1.Manifestos.Items[0].Alertas;
              ACBrMDFe1.Manifestos.Assinar;
              ACBrMDFe1.Manifestos.Validar;
              ArqMDFe := PathWithDelim(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(1)))+OnlyNumber(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID)+'-mdfe.xml';
              ACBrMDFe1.Manifestos.GravarXML(ExtractFilePath(ArqMDFe));
              if not FileExists(ArqMDFe) then
                 raise Exception.Create('Não foi possível criar o arquivo '+ArqMDFe);
            end
           else if (Cmd.Metodo = 'criarmdfe')  or (Cmd.Metodo = 'criaresefaz') or
           (Cmd.Metodo = 'criarenviarmdfe') or (Cmd.Metodo = 'criarenviarmdfesefaz') then
            begin
              Salva := ACBrMDFe1.Configuracoes.Geral.Salvar;
              if not Salva then
               begin
                ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
                ACBrMDFe1.Configuracoes.Arquivos.PathSalvar := PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs';
               end;
              ACBrMDFe1.Manifestos.GerarMDFe;
              Alertas := ACBrMDFe1.Manifestos.Items[0].Alertas;
              ACBrMDFe1.Manifestos.Assinar;
              ACBrMDFe1.Manifestos.Validar;
              ArqMDFe := PathWithDelim(ACBrMDFe1.Configuracoes.Arquivos.PathSalvar)+OnlyNumber(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID)+'-mdfe.xml';
              ACBrMDFe1.Manifestos.GravarXML(ArqMDFe);
              if not FileExists(ArqMDFe) then
                raise Exception.Create('Não foi possível criar o arquivo '+ArqMDFe);
            end;

           Cmd.Resposta := ArqMDFe;
           if Alertas <> '' then
              Cmd.Resposta :=  Cmd.Resposta+sLineBreak+'Alertas:'+Alertas;
           if ((Cmd.Metodo = 'criarmdfe') or (Cmd.Metodo = 'criarmdfesefaz')) and (Cmd.Params(1) = '1') then
            begin
              SL := TStringList.Create;
              SL.LoadFromFile(ArqMDFe);
              Cmd.Resposta :=  Cmd.Resposta+sLineBreak+SL.Text;
              SL.Free;
            end;

           if (Cmd.Metodo = 'criarenviarmdfe') or (Cmd.Metodo = 'criarenviarmdfesefaz') or (Cmd.Metodo = 'enviarlotemdfe') then
            begin
              //Carregar Notas quando enviar lote
              if (Cmd.Metodo = 'enviarlotemdfe')   then
               begin
                 if not DirectoryExists(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(0))) then
                    raise Exception.Create('Diretório não encontrado:'+PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(0)))
                 else
                  begin
                    ACBrMDFe1.Manifestos.Clear;
                    RetFind := SysUtils.FindFirst( PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+Cmd.Params(0)+PathDelim+'*-mdfe.xml', faAnyFile, SearchRec);
                    if (RetFind = 0) then
                     begin
                       while RetFind = 0 do
                        begin
                           ACBrMDFe1.Manifestos.LoadFromFile(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+Cmd.Params(0)+PathDelim+SearchRec.Name);
                           RetFind := FindNext(SearchRec);
                        end;
                        ACBrMDFe1.Manifestos.GerarMDFe;
                        ACBrMDFe1.Manifestos.Assinar;
                        ACBrMDFe1.Manifestos.Validar;
                     end
                    else
                       raise Exception.Create('Não foi encontrada nenhuma nota para o Lote: '+Cmd.Params(0) );
                  end;
               end;

                 if not(ACBrMDFe1.WebServices.StatusServico.Executar) then
                  raise Exception.Create(ACBrMDFe1.WebServices.StatusServico.Msg);

                 if (Cmd.Metodo = 'criarenviarmdfe') or (Cmd.Metodo = 'criarenviarmdfesefaz') then
                  begin
                    if Trim(OnlyNumber(Cmd.Params(1))) = '' then
                       ACBrMDFe1.WebServices.Enviar.Lote := '1'
                    else
                       ACBrMDFe1.WebServices.Enviar.Lote := OnlyNumber(Cmd.Params(1)); //StrToIntDef( OnlyNumber(Cmd.Params(1)),1);
                  end
                 else
                  begin
                    if Trim(OnlyNumber(Cmd.Params(0))) = '' then
                       ACBrMDFe1.WebServices.Enviar.Lote := '1'
                    else
                       ACBrMDFe1.WebServices.Enviar.Lote := OnlyNumber(Cmd.Params(0)); //StrToIntDef( OnlyNumber(Cmd.Params(0)),1);
                  end;
                 ACBrMDFe1.WebServices.Enviar.Executar;

                 Cmd.Resposta :=  ACBrMDFe1.WebServices.Enviar.Msg+sLineBreak+
                                 '[ENVIO]'+sLineBreak+
                                 'Versao='+ACBrMDFe1.WebServices.Enviar.verAplic+sLineBreak+
                                 'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.Enviar.TpAmb)+sLineBreak+
                                 'VerAplic='+ACBrMDFe1.WebServices.Enviar.VerAplic+sLineBreak+
                                 'CStat='+IntToStr(ACBrMDFe1.WebServices.Enviar.CStat)+sLineBreak+
                                 'XMotivo='+ACBrMDFe1.WebServices.Enviar.XMotivo+sLineBreak+
                                 'CUF='+IntToStr(ACBrMDFe1.WebServices.Enviar.CUF)+sLineBreak+
                                 'NRec='+ACBrMDFe1.WebServices.Enviar.Recibo+sLineBreak+
                                 'DhRecbto='+DateTimeToStr(ACBrMDFe1.WebServices.Enviar.dhRecbto)+sLineBreak+
                                 'TMed='+IntToStr(ACBrMDFe1.WebServices.Enviar.TMed)+sLineBreak+
                                 'Msg='+ACBrMDFe1.WebServices.Enviar.Msg+sLineBreak;

                 ACBrMDFe1.WebServices.Retorno.Recibo := ACBrMDFe1.WebServices.Enviar.Recibo;
                 ACBrMDFe1.WebServices.Retorno.Executar;

                 Cmd.Resposta :=  Cmd.Resposta+
                                  ACBrMDFe1.WebServices.Retorno.Msg+sLineBreak+
                                  '[RETORNO]'+sLineBreak+
                                  'Versao='+ACBrMDFe1.WebServices.Retorno.verAplic+sLineBreak+
                                  'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.Retorno.TpAmb)+sLineBreak+
                                  'VerAplic='+ACBrMDFe1.WebServices.Retorno.VerAplic+sLineBreak+
                                  'NRec='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.nRec+sLineBreak+
                                  'CStat='+IntToStr(ACBrMDFe1.WebServices.Retorno.CStat)+sLineBreak+
                                  'XMotivo='+ACBrMDFe1.WebServices.Retorno.XMotivo+sLineBreak+
                                  'CUF='+IntToStr(ACBrMDFe1.WebServices.Retorno.CUF)+sLineBreak;

                 for I:= 0 to ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Count-1 do
                  begin
                   for J:= 0 to ACBrMDFe1.Manifestos.Count-1 do
                    begin
                     if 'MDFe'+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].chMDFe = ACBrMDFe1.Manifestos.Items[j].MDFe.infMDFe.Id  then
                      begin
                        Cmd.Resposta := Cmd.Resposta+
                                   '[MDFe'+Trim(IntToStr(ACBrMDFe1.Manifestos.Items[j].MDFe.Ide.nMDF))+']'+sLineBreak+
                                   'Versao='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].verAplic+sLineBreak+
                                   'TpAmb='+TpAmbToStr(ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].tpAmb)+sLineBreak+
                                   'VerAplic='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].verAplic+sLineBreak+
                                   'CStat='+IntToStr(ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].cStat)+sLineBreak+
                                   'XMotivo='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].xMotivo+sLineBreak+
                                   'CUF='+IntToStr(ACBrMDFe1.WebServices.Retorno.MDFeRetorno.cUF)+sLineBreak+
                                   'ChMDFe='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].chMDFe+sLineBreak+
                                   'DhRecbto='+DateTimeToStr(ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].dhRecbto)+sLineBreak+
                                   'NProt='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].nProt+sLineBreak+
                                   'DigVal='+ACBrMDFe1.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].digVal+sLineBreak+
                                   'Arquivo='+PathWithDelim(ACBrMDFe1.Configuracoes.Arquivos.PathSalvar)+OnlyNumber(ACBrMDFe1.Manifestos.Items[j].MDFe.infMDFe.ID)+'-MDFe.xml'+sLineBreak;

                        ACBrMDFe1.DAMDFe.Impressora := cbxImpressora.Text;
                        if ACBrMDFe1.Manifestos.Items[i].Confirmado and (Cmd.Params(2) = '1') then
                         begin
                           try
                             AntesDeImprimir((Cmd.Params(2) = '1') and ACBrMDFe1.DAMDFe.MostrarPreview);
                             ACBrMDFe1.Manifestos.Items[i].Imprimir;
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
        else if Cmd.Metodo = 'enviaremail' then
         begin
           ACBrMDFe1.Manifestos.Clear;
           PathsMDFe := TStringList.Create;
           try
             PathsMDFe.Append(Cmd.Params(1));
             PathsMDFe.Append(PathWithDelim(ACBrMDFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
             try
               CarregarDFe(PathsMDFe, ArqMDFe, tDFeMDFe);
             except
             end;
           finally
             PathsMDFe.Free;
           end;

           sMensagemEmail := TStringList.Create;
           CC := TstringList.Create;
           Anexos := TstringList.Create;

           try
             sMensagemEmail.Text := mmEmailMsgMDFe.Lines.Text;

             CC.DelimitedText := sLineBreak;
             CC.Text := StringReplace(Cmd.Params(4),';',sLineBreak,[rfReplaceAll]);

             Anexos.DelimitedText := sLineBreak;
             Anexos.Text := StringReplace(Cmd.Params(5),';',sLineBreak,[rfReplaceAll]);
             try
               ACBrMDFe1.Manifestos.Items[0].EnviarEmail( Cmd.Params(0),
                                                          IfThen(NaoEstaVazio(Cmd.Params(3)),Cmd.Params(3),edtEmailAssuntoMDFe.Text),
                                                           sMensagemEmail,
                                                           (Cmd.Params(2) = '1'),   // Enviar PDF junto
                                                           CC, // Lista com emails que serão enviado cópias - TStrings
                                                           Anexos); // Lista de anexos - TStrings


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

        else if Cmd.Metodo = 'setcertificado' then
         begin
           DoACBr(Cmd);
         end

        else if Cmd.Metodo = 'setambiente' then //1-Produção 2-Homologação
        begin
           if (StrToInt(Cmd.Params(0))>=1) and (StrToInt(Cmd.Params(0))<=2) then
            begin
              ACBrMDFe1.Configuracoes.WebServices.Ambiente := StrToTpAmb(OK, Cmd.Params(0));
              rgTipoAmb.ItemIndex := ACBrMDFe1.Configuracoes.WebServices.AmbienteCodigo-1;
              FrmACBrMonitor.SalvarIni;
            end
           else
              raise Exception.Create('Ambiente Inválido.');
        end

        else if Cmd.Metodo = 'setlogomarca' then
        begin
          if FileExists(Cmd.Params(0)) then
           begin
             ACBrMDFe1.DAMDFE.Logo     := Cmd.Params(0);
             edtLogoMarca.Text         := ACBrMDFe1.DAMDFE.Logo;
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
             ACBrMDFe1.Configuracoes.Geral.FormaEmissao := StrToTpEmis(OK, Cmd.Params(0));
             cbFormaEmissaoMDFe.ItemIndex := ACBrMDFe1.Configuracoes.Geral.FormaEmissaoCodigo-1;
             SalvarIni;
           end;
         end

        else if Cmd.Metodo = 'setversaodf' then //MDFe.SetVersaoDF(nVersao) 1.00 3.00
        begin
          VersaoDF := StrToVersaoMDFe(OK, Cmd.Params(0));
          if OK then
           begin
             ACBrMDFe1.Configuracoes.Geral.VersaoDF := VersaoDF;
             cbVersaoWSMDFe.ItemIndex := cbVersaoWSMDFe.Items.IndexOf(Cmd.Params(0)) ;
             SalvarIni;
           end
          else
            raise Exception.Create('Versão Inválida.');
        end

        else if Cmd.Metodo = 'lermdfe' then
         begin
           try
              Cmd.Resposta := GerarMDFeIni( Cmd.Params(0));
           except
             on E: Exception do
             begin
               raise Exception.Create('Erro ao gerar INI do MDFe.'+sLineBreak+E.Message);
             end;
           end;
         end

        else if Cmd.Metodo = 'mdfetotxt' then  //1-Arquivo XML, 2-NomeArqTXT
         begin
           ACBrMDFe1.Manifestos.Clear;
           CarregarDFe(Cmd.Params(0), ArqMDFe, tDFeMDFe);
           ACBrMDFe1.Manifestos.Items[0].GravarXML(Cmd.Params(1));

           Cmd.Resposta := ChangeFileExt(ACBrMDFe1.Manifestos.Items[0].NomeArq,'.txt');
         end

        else if Cmd.Metodo = 'savetofile' then
         begin
           DoACbr(Cmd);
         end

        else if Cmd.Metodo = 'loadfromfile' then
         begin
           DoACBr(Cmd);
         end

        else if Cmd.Metodo = 'fileexists' then
         begin
           if not FileExists( Cmd.Params(0) ) then
              raise Exception.Create('Arquivo '+Cmd.Params(0)+' não encontrado')
         end


        else if Cmd.Metodo = 'certificadodatavencimento' then
         begin
           Cmd.Resposta := DateToStr(ACBrMDFe1.SSL.CertDataVenc);
         end

        else if Cmd.Metodo = 'lerini' then // Recarrega configurações do arquivo INI
           FrmACBrMonitor.LerIni

        else if Cmd.Metodo = 'gerarchave' then
         begin
           GerarChave(Chave,
                      StrToInt(Cmd.Params(0)), //codigoUF
                      StrToInt(Cmd.Params(1)), //codigoNumerico
                      StrToInt(Cmd.Params(2)), //modelo
                      StrToInt(Cmd.Params(3)), //serie
                      StrToInt(Cmd.Params(4)), //numero
                      StrToInt(Cmd.Params(5)), //tpemi
                      StringToDateTime(Cmd.Params(6)), //emissao
                      Cmd.Params(7)); //CNPJ
           Cmd.Resposta := Chave;
         end

        else if Cmd.Metodo = 'restaurar' then
           Restaurar1Click( FrmACBrMonitor )

        else if Cmd.Metodo = 'ocultar' then
           Ocultar1Click( FrmACBrMonitor )

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


procedure GerarIniMDFe( AStr: String );
var
  I, J, K, L, M : Integer;
  sSecao, sFim : String;
  INIRec : TMemIniFile;
  OK     : boolean;
begin
  INIRec := LerConverterIni(AStr);

  with FrmACBrMonitor do
  begin
    try
      ACBrMDFe1.Manifestos.Clear;
      with ACBrMDFe1.Manifestos.Add.MDFe do
       begin
         OK := True;
         Ide.tpEmit  := StrToTpEmitente(OK, INIRec.ReadString('ide', 'tpEmit', '1'));
         Ide.modelo  := INIRec.ReadString('ide', 'mod', '58');
         Ide.serie   := INIRec.ReadInteger('ide', 'serie', 1);
         Ide.nMDF    := INIRec.ReadInteger('ide', 'nMDF', 0);
         Ide.cMDF    := INIRec.ReadInteger('ide', 'cMDF', 0);
         Ide.modal   := StrToModal(OK, INIRec.ReadString('ide', 'modal', '01'));
         Ide.dhEmi   := StringToDateTime(INIRec.ReadString('ide', 'dhEmi', '0'));
         Ide.tpEmis  := StrToTpEmis(OK, INIRec.ReadString('ide', 'tpEmis', IntToStr(ACBrMDFe1.Configuracoes.Geral.FormaEmissaoCodigo)));
         Ide.procEmi := StrToProcEmi(OK, INIRec.ReadString('ide', 'procEmi', '0'));
         Ide.verProc := INIRec.ReadString('ide', 'verProc', 'ACBrMDFe');
         Ide.UFIni   := INIRec.ReadString('ide', 'UFIni', '');
         Ide.UFFim   := INIRec.ReadString('ide', 'UFFim', '');
         Ide.tpTransp:= StrToTTransportador(OK, INIRec.ReadString('ide', 'tpTransp', '1'));

         I := 1;
         while true do
         begin
           sSecao := 'CARR' + IntToStrZero(I, 3);
           sFim   := INIRec.ReadString(sSecao, 'xMunCarrega', 'FIM');
           if (sFim = 'FIM') or (Length(sFim) <= 0) then
             break;
           with Ide.infMunCarrega.Add do
           begin
             cMunCarrega := INIRec.ReadInteger(sSecao, 'cMunCarrega', 0);
             xMunCarrega := sFim;
           end;
           Inc(I);
         end;

         I := 1;
         while true do
         begin
           sSecao := 'PERC' + IntToStrZero(I, 3);
           sFim   := INIRec.ReadString(sSecao, 'UFPer', 'FIM');
           if (sFim = 'FIM') or (Length(sFim) <= 0) then
             break;
           with Ide.infPercurso.Add do
           begin
             UFPer := sFim;
           end;
           Inc(I);
         end;

         Ide.dhIniViagem := StringToDateTime(INIRec.ReadString('ide', 'dhIniViagem', '0'));

         Emit.CNPJ  := INIRec.ReadString('emit', 'CNPJ', '');
         Emit.IE    := INIRec.ReadString('emit', 'IE', '');
         Emit.xNome := INIRec.ReadString('emit', 'xNome', '');
         Emit.xFant := INIRec.ReadString('emit', 'xFant', '');

         Emit.enderEmit.xLgr    := INIRec.ReadString('emit', 'xLgr', '');
         Emit.enderEmit.nro     := INIRec.ReadString('emit', 'nro', '');
         Emit.enderEmit.xCpl    := INIRec.ReadString('emit', 'xCpl', '');
         Emit.enderEmit.xBairro := INIRec.ReadString('emit', 'xBairro', '');
         Emit.enderEmit.cMun    := INIRec.ReadInteger('emit', 'cMun', 0);
         Emit.enderEmit.xMun    := INIRec.ReadString('emit', 'xMun', '');
         Emit.enderEmit.CEP     := INIRec.ReadInteger('emit', 'CEP', 0);
         Emit.enderEmit.UF      := INIRec.ReadString('emit', 'UF', '');
         Emit.enderEmit.fone    := INIRec.ReadString('emit', 'fone', '');
         Emit.enderEmit.email   := INIRec.ReadString('emit', 'email', '');

         ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigo(Emit.enderEmit.UF));

         //*********************************************************************
         //
         // Modal Rodoviário
         //
         //*********************************************************************

         rodo.RNTRC              := INIRec.ReadString('Rodo', 'RNTRC', '');
         rodo.infANTT.RNTRC      := INIRec.ReadString('infANTT', 'RNTRC', '');

         if ( (rodo.RNTRC <> '') or (rodo.infANTT.RNTRC <> '') )  then
         begin
           rodo.CIOT               := INIRec.ReadString('Rodo', 'CIOT', '');
           rodo.veicTracao.cInt    := INIRec.ReadString('veicTracao', 'cInt', '');
           rodo.veicTracao.placa   := INIRec.ReadString('veicTracao', 'placa', '');
           rodo.veicTracao.RENAVAM := INIRec.ReadString('veicTracao', 'RENAVAM', '');
           rodo.veicTracao.tara    := INIRec.ReadInteger('veicTracao', 'tara', 0);
           rodo.veicTracao.capKG   := INIRec.ReadInteger('veicTracao', 'capKG', 0);
           rodo.veicTracao.capM3   := INIRec.ReadInteger('veicTracao', 'capM3', 0);

           // Dados da ANTT MDFe Versão 3.0

           if (rodo.infANTT.RNTRC <> '') then
           begin
             I := 1;
             while true do
             begin
               sSecao := 'infCIOT' + IntToStrZero(I, 3);
               sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');
               if sFim = 'FIM' then
                 break;

               with rodo.infANTT.infCIOT.Add do
               begin
                 CIOT    := INIRec.ReadString(sSecao, 'CIOT', '');
                 CNPJCPF := sFim;
               end;
               Inc(I);
             end;

             I := 1;
             while true do
             begin
               sSecao := 'valePed' + IntToStrZero(I, 3);
               sFim   := INIRec.ReadString(sSecao, 'CNPJForn', 'FIM');
               if sFim = 'FIM' then
                 break;

               with rodo.infANTT.valePed.disp.Add do
               begin
                 CNPJForn    := sFim;
                 CNPJPg      := INIRec.ReadString(sSecao, 'CNPJPg', '');
                 nCompra     := INIRec.ReadString(sSecao, 'nCompra', '');
                 vValePed    := StringToFloatDef(INIRec.ReadString(sSecao, 'vValePed', ''), 0 );
               end;
               Inc(I);
             end;

             I := 1;
             while true do
             begin
               sSecao := 'infContratante' + IntToStrZero(I, 3);
               sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');
               if sFim = 'FIM' then
                 break;

               with rodo.infANTT.infContratante.Add do
               begin
                 CNPJCPF := sFim;
               end;
               Inc(I);
             end;

           end;
           // Dados do proprietário do veículo de Tração

           if INIRec.ReadString('veicTracao', 'CNPJCPF', '') <> '' then
           begin
             rodo.veicTracao.prop.CNPJCPF := INIRec.ReadString('veicTracao', 'CNPJCPF', '');
             rodo.veicTracao.prop.RNTRC   := INIRec.ReadString('veicTracao', 'RNTRC', '');
             rodo.veicTracao.prop.xNome   := INIRec.ReadString('veicTracao', 'xNome', '');
             rodo.veicTracao.prop.IE      := INIRec.ReadString('veicTracao', 'IE', '');
             rodo.veicTracao.prop.UF      := INIRec.ReadString('veicTracao', 'UFProp', '');
             rodo.veicTracao.prop.tpProp  := StrToTpProp(OK, INIRec.ReadString('veicTracao', 'tpProp', '0'));
           end;

           I := 1;
           while true do
           begin
             sSecao := 'moto' + IntToStrZero(I, 3);
             sFim   := INIRec.ReadString(sSecao, 'xNome', 'FIM');
             if sFim = 'FIM' then
               break;
             with rodo.veicTracao.condutor.Add do
             begin
               xNome := sFim;
               CPF   := INIRec.ReadString(sSecao, 'CPF', '');
             end;
             Inc(I);
           end;

           rodo.veicTracao.tpRod := StrToTpRodado(OK, INIRec.ReadString('veicTracao', 'tpRod', '01'));
           rodo.veicTracao.tpCar := StrToTpCarroceria(OK, INIRec.ReadString('veicTracao', 'tpCar', '00'));
           rodo.veicTracao.UF    := INIRec.ReadString('veicTracao', 'UF', '');

           I := 1;
           while true do
           begin
             sSecao := 'reboque' + IntToStrZero(I, 2);
             sFim   := INIRec.ReadString(sSecao, 'placa', 'FIM');
             if sFim = 'FIM' then
               break;
             with rodo.veicReboque.Add do
             begin
               cInt    := INIRec.ReadString(sSecao, 'cInt', '');
               placa   := sFim;
               RENAVAM := INIRec.ReadString(sSecao, 'RENAVAM', '');
               tara    := INIRec.ReadInteger(sSecao, 'tara', 0);
               capKG   := INIRec.ReadInteger(sSecao, 'capKG', 0);
               capM3   := INIRec.ReadInteger(sSecao, 'capM3', 0);

               // Dados do proprietário do veículo Reboque

               if INIRec.ReadString(sSecao, 'CNPJCPF', '') <> '' then
               begin
                 prop.CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');
                 prop.RNTRC   := INIRec.ReadString(sSecao, 'RNTRC', '');
                 prop.xNome   := INIRec.ReadString(sSecao, 'xNome', '');
                 prop.IE      := INIRec.ReadString(sSecao, 'IE', '');
                 prop.UF      := INIRec.ReadString(sSecao, 'UFProp', '');
                 prop.tpProp  := StrToTpProp(OK, INIRec.ReadString(sSecao, 'tpProp', '0'));
               end;

               tpCar := StrToTpCarroceria(OK, INIRec.ReadString(sSecao, 'tpCar', '00'));
               UF    := INIRec.ReadString(sSecao, 'UF', '');
             end;
             Inc(I);
           end;

           I := 1;
           while true do
           begin
             sSecao := 'valePed' + IntToStrZero(I, 3);
             sFim   := INIRec.ReadString(sSecao, 'CNPJForn', 'FIM');
             if sFim = 'FIM' then
               break;
             with rodo.valePed.disp.Add do
             begin
               CNPJForn := sFim;
               CNPJPg   := INIRec.ReadString(sSecao, 'CNPJPg', '');
               nCompra  := INIRec.ReadString(sSecao, 'nCompra', '');
             end;
             Inc(I);
           end;

           Rodo.codAgPorto := INIRec.ReadString('Rodo', 'codAgPorto', '');
         end; // Fim do Rodoviário

         //*********************************************************************
         //
         // Modal Aéreo
         //
         //*********************************************************************

         Aereo.nac     := INIRec.ReadInteger('aereo', 'nac', 0);
         if (Aereo.nac <> 0) then
         begin
           Aereo.matr    := INIRec.ReadInteger('aereo', 'matr', 0);
           Aereo.nVoo    := INIRec.ReadString('aereo', 'nVoo', '');
           Aereo.cAerEmb := INIRec.ReadString('aereo', 'cAerEmb', '');
           Aereo.cAerDes := INIRec.ReadString('aereo', 'cAerDes', '');
           Aereo.dVoo    := StringToDateTime(INIRec.ReadString('aereo', 'dVoo', '0'));
         end; // Fim do Aereoviário

         //*********************************************************************
         //
         // Modal Aquaviário
         //
         //*********************************************************************

         Aquav.CNPJAgeNav  := INIRec.ReadString('aquav', 'CNPJAgeNav', '');
         Aquav.irin        := INIRec.ReadString('aquav', 'irin', '');

         if ( (Aquav.CNPJAgeNav  <> '') or (Aquav.irin <> '') ) then
         begin
           Aquav.tpEmb      := INIRec.ReadString('aquav', 'tpEmb', '');
           Aquav.cEmbar     := INIRec.ReadString('aquav', 'cEmbar', '');
           Aquav.xEmbar     := INIRec.ReadString('aquav', 'xEmbar', '');
           Aquav.nViagem    := INIRec.ReadString('aquav', 'nViag', '');
           Aquav.cPrtEmb    := INIRec.ReadString('aquav', 'cPrtEmb', '');
           Aquav.cPrtDest   := INIRec.ReadString('aquav', 'cPrtDest', '');

           //Campos MDF-e 3.0
           Aquav.prtTrans    := INIRec.ReadString('aquav', 'prtTrans', '');
           Aquav.tpNav       := StrToTpNavegacao(OK, INIRec.ReadString('aquav', 'tpNav', '0') );

           I := 1;
           while true do
           begin
             sSecao := 'infTermCarreg' + IntToStrZero(I, 1);
             sFim   := INIRec.ReadString(sSecao, 'cTermCarreg', 'FIM');
             if sFim = 'FIM' then
               break;
             with Aquav.infTermCarreg.Add do
             begin
               cTermCarreg := sFim;
               xTermCarreg := INIRec.ReadString(sSecao, 'xTermCarreg', '');
             end;
             inc(I);
           end;

           I := 1;
           while true do
           begin
             sSecao := 'infTermDescarreg' + IntToStrZero(I, 1);
             sFim   := INIRec.ReadString(sSecao, 'cTermDescarreg', 'FIM');
             if sFim = 'FIM' then
               break;
             with Aquav.infTermDescarreg.Add do
             begin
               cTermDescarreg := sFim;
               xTermDescarreg := INIRec.ReadString(sSecao, 'xTermDescarreg', '');
             end;
             inc(I);
           end;

           I := 1;
           while true do
           begin
             sSecao := 'infEmbComb' + IntToStrZero(I, 2);
             sFim   := INIRec.ReadString(sSecao, 'cEmbComb', 'FIM');
             if sFim = 'FIM' then
               break;
             with Aquav.infEmbComb.Add do
             begin
               cEmbComb := sFim;
               xBalsa   :=  INIRec.ReadString(sSecao, 'xBalsa', '');
             end;
             inc(I);
           end;

           I := 1;
           while true do
           begin
             sSecao := 'infUnidCargaVazia' + IntToStrZero(I, 3);
             sFim   := INIRec.ReadString(sSecao, 'idUnidCargaVazia', 'FIM');
             if sFim = 'FIM' then
               break;
             with Aquav.infUnidCargaVazia.Add do
             begin
               idUnidCargaVazia := sFim;
               tpUnidCargaVazia := StrToUnidCarga(OK, INIRec.ReadString(sSecao, 'tpUnidCargaVazia', '1'));
             end;
             inc(I);
           end;

           I := 1;
           while true do
           begin
             sSecao := 'infUnidTranspVazia' + IntToStrZero(I, 3);
             sFim   := INIRec.ReadString(sSecao, 'idUnidTranspVazia', 'FIM');
             if sFim = 'FIM' then
               break;
             with Aquav.infUnidTranspVazia.Add do
             begin
               idUnidTranspVazia := sFim;
               tpUnidTranspVazia := StrToUnidTransp (OK, INIRec.ReadString(sSecao, 'tpUnidTranspVazia', '1'));
             end;
             inc(I);
           end;

         end; // Fim do Aquaviário

         //*********************************************************************
         //
         // Modal Ferroviário
         //
         //*********************************************************************

         Ferrov.xPref  := INIRec.ReadString('ferrov', 'xPref', '');
         if (Ferrov.xPref <> '') then
         begin
           Ferrov.dhTrem := StringToDateTime(INIRec.ReadString('ferrov', 'dhTrem', '0'));
           Ferrov.xOri   := INIRec.ReadString('ferrov', 'xOri', '');
           Ferrov.xDest  := INIRec.ReadString('ferrov', 'xDest', '');
           Ferrov.qVag   := INIRec.ReadInteger('ferrov', 'qVag', 0);

           I := 1;
           while true do
           begin
             sSecao := 'vag' + IntToStrZero(I, 3);
             sFim   := INIRec.ReadString(sSecao, 'serie', 'FIM');
             if sFim = 'FIM' then
               break;
             with Ferrov.vag.Add do
             begin
               serie := sFim;
               nVag  := INIRec.ReadInteger(sSecao, 'nVag', 0);
               nSeq  := INIRec.ReadInteger(sSecao, 'nSeq', 0);
               TU    := StringToFloatDef(INIRec.ReadString(sSecao, 'TU', ''), 0);

               //Campos MDF-e 3.0
               pesoBC   := StringToFloatDef( INIRec.ReadString(sSecao, 'pesoBC', ''), 0);
               pesoR    := StringToFloatDef( INIRec.ReadString(sSecao, 'pesoR', ''), 0);
               tpVag    := INIRec.ReadString(sSecao, 'tpVag', '');

             end;
             inc(I);
           end;
         end; // Fim do Ferroviário

         I := 1;
         while true do
         begin
           sSecao := 'DESC' + IntToStrZero(I, 3);
           sFim   := INIRec.ReadString(sSecao, 'xMunDescarga', 'FIM');
           if (sFim = 'FIM') or (Length(sFim) <= 0) then
             break;
           with infDoc.infMunDescarga.Add do
           begin
             cMunDescarga := INIRec.ReadInteger(sSecao, 'cMunDescarga', 0);
             xMunDescarga := sFim;

             J := 1;
             while true do
             begin
               sSecao := 'infCTe' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
               sFim   := INIRec.ReadString(sSecao, 'chCTe', 'FIM');
               if sFim = 'FIM' then
                 break;
               with infCTe.Add do
               begin
                 chCTe       := sFim;
                 SegCodBarra := INIRec.ReadString(sSecao, 'SegCodBarra', '');
                 indReentrega:= INIRec.ReadString(sSecao, 'indReentrega', '');
                 K := 1;
                 while true do
                 begin
                   sSecao := 'peri'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                   sFim   := INIRec.ReadString(sSecao,'nONU','FIM');
                   if sFim = 'FIM' then
                     break;

                   with peri.Add do
                   begin
                     nONU        := sFim;
                     xNomeAE     := INIRec.ReadString(sSecao,'xNomeAE','');
                     xClaRisco   := INIRec.ReadString(sSecao,'xClaRisco','');
                     grEmb       := INIRec.ReadString(sSecao,'grEmb','');
                     qTotProd    := INIRec.ReadString(sSecao,'qTotProd','');
                     qVolTipo    := INIRec.ReadString(sSecao,'qVolTipo','');

                   end;
                   inc(K);
                 end;

                 K := 1;
                 while true do
                 begin
                   sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                   sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');
                   if sFim = 'FIM' then
                     break;

                   with infUnidTransp.Add do
                   begin
                     tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
                     idUnidTransp := sFim;
                     qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                     L := 1;
                     while true do
                     begin
                       sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                       sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');
                       if sFim = 'FIM' then
                         break;

                       with lacUnidTransp.Add do
                       begin
                         nLacre := sFim;
                       end;
                       inc(L);
                     end;

                     L := 1;
                     while true do
                     begin
                       sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                       sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

                       if sFim = 'FIM' then
                         break;

                       with infUnidCarga.Add do
                       begin
                         tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                         idUnidCarga := sFim;


                         qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);
                         M := 1;
                         while true do
                         begin
                           sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3)+IntToStrZero(M,3);
                           sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                           if sFim = 'FIM' then
                             break;

                           with lacUnidCarga.Add do
                           begin
                             nLacre := sFim;
                           end;

                           inc(M);
                         end;
                       end;
                       inc(L);
                     end;
                   end;
                   inc(K);
                 end;

               end;
               Inc(J);
             end;

             J := 1;
             while true do
             begin
               sSecao := 'infNFe' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
               sFim   := INIRec.ReadString(sSecao, 'chNFe', 'FIM');

               if sFim = 'FIM' then
                 break;

               with infNFe.Add do
               begin
                 chNFe       := sFim;
                 SegCodBarra := INIRec.ReadString(sSecao, 'SegCodBarra', '');
                 indReentrega:= INIRec.ReadString(sSecao, 'indReentrega', '');

                 K := 1;
                 while true do
                 begin
                   sSecao := 'peri'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                   sFim   := INIRec.ReadString(sSecao,'nONU','FIM');
                   if sFim = 'FIM' then
                     break;

                   with peri.Add do
                   begin
                     nONU        := sFim;
                     xNomeAE     := INIRec.ReadString(sSecao,'xNomeAE','');
                     xClaRisco   := INIRec.ReadString(sSecao,'xClaRisco','');
                     grEmb       := INIRec.ReadString(sSecao,'grEmb','');
                     qTotProd    := INIRec.ReadString(sSecao,'qTotProd','');
                     qVolTipo    := INIRec.ReadString(sSecao,'qVolTipo','');

                   end;
                   inc(K);
                 end;

                 K := 1;
                 while true do
                 begin
                   sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                   sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');

                   if sFim = 'FIM' then
                     break;

                   with infUnidTransp.Add do
                   begin
                     tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
                     idUnidTransp := sFim;
                     qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                     L := 1;
                     while true do
                     begin
                       sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                       sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                       if sFim = 'FIM' then
                         break;

                       with lacUnidTransp.Add do
                       begin
                         nLacre := sFim;
                       end;
                       inc(L);
                     end;

                     L := 1;
                     while true do
                     begin
                       sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                       sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

                       if sFim = 'FIM' then
                         break;

                       with infUnidCarga.Add do
                       begin
                         tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                         idUnidCarga := sFim;
                         qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                         M := 1;
                         while true do
                         begin
                           sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3)+IntToStrZero(M,3);
                           sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                           if sFim = 'FIM' then
                             break;

                           with lacUnidCarga.Add do
                           begin
                             nLacre := sFim;
                           end;

                           inc(M);
                         end;
                       end;
                       inc(L);
                     end;
                   end;
                   inc(K);
                 end;
               end;
               Inc(J);
             end;

             J := 1;
             while true do
             begin
               sSecao := 'infMDFeTransp' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
               sFim   := INIRec.ReadString(sSecao, 'chMDFe', 'FIM');
               if sFim = 'FIM' then
                 break;

               with infMDFeTransp.Add do
               begin
                 chMDFe := sFim;
                 indReentrega:= INIRec.ReadString(sSecao, 'indReentrega', '');

                 K := 1;
                 while true do
                 begin
                   sSecao := 'peri'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                   sFim   := INIRec.ReadString(sSecao,'nONU','FIM');
                   if sFim = 'FIM' then
                     break;

                   with peri.Add do
                   begin
                     nONU        := sFim;
                     xNomeAE     := INIRec.ReadString(sSecao,'xNomeAE','');
                     xClaRisco   := INIRec.ReadString(sSecao,'xClaRisco','');
                     grEmb       := INIRec.ReadString(sSecao,'grEmb','');
                     qTotProd    := INIRec.ReadString(sSecao,'qTotProd','');
                     qVolTipo    := INIRec.ReadString(sSecao,'qVolTipo','');

                   end;
                   inc(K);
                 end;

                 K := 1;
                 while true do
                 begin
                   sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
                   sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');
                   if sFim = 'FIM' then
                       break;

                   with infUnidTransp.Add do
                   begin
                     tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
                     idUnidTransp := sFim;
                     qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

                     L := 1;
                     while true do
                     begin
                       sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                       sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                       if sFim = 'FIM' then
                         break;


                       with lacUnidTransp.Add do
                       begin
                         nLacre := sFim;
                       end;

                       inc(L);
                     end;

                     L := 1;
                     while true do
                     begin
                       sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
                       sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

                       if sFim = 'FIM' then
                         break;

                       with infUnidCarga.Add do
                       begin
                         tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
                         idUnidCarga := sFim;

                         qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);
                         M := 1;
                         while true do
                         begin
                           sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3)+IntToStrZero(M,3);
                           sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                           if sFim = 'FIM' then
                             break;

                           with lacUnidCarga.Add do
                           begin
                             nLacre := sFim;
                           end;

                           inc(M);
                         end;

                         inc(L);
                       end;
                     end;
                   inc(K);
                 end;

               end;
               Inc(J);
             end;
           end;

           Inc(I);
         end;
       end;

       I := 1;
       while true do
       begin
         sSecao := 'seg' + IntToStrZero(I, 3);
         sFim   := INIRec.ReadString(sSecao, 'CNPJ', 'FIM');
         if sFim = 'FIM' then
           break;
         with seg.Add do
         begin
           respSeg    :=  StrToRspSeguroMDFe(OK, INIRec.ReadString(sSecao, 'respSeg', '1'));
           CNPJCPF    := INIRec.ReadString(sSecao, 'CNPJCPF', '');
           xSeg       := INIRec.ReadString(sSecao, 'xSeg', '');
           CNPJ       := sFim;
           nApol      := INIRec.ReadString(sSecao, 'nApol', '');

           J := 1;
           while true do
           begin
             sSecao := 'aver' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
             sFim   := INIRec.ReadString(sSecao, 'nAver', 'FIM');
             if sFim = 'FIM' then
               break;
             with aver.Add do
             begin
               nAver      := sFim;
             end;
             Inc(J);
           end;

         end;
         Inc(I);
       end;

       tot.qCTe   := INIRec.ReadInteger('tot', 'qCTe', 0);
       tot.qCT    := INIRec.ReadInteger('tot', 'qCT', 0);
       tot.qNFe   := INIRec.ReadInteger('tot', 'qNFe', 0);
       tot.qNF    := INIRec.ReadInteger('tot', 'qNF', 0);
       tot.qMDFe  := INIRec.ReadInteger('tot', 'qMDFe', 0);
       tot.vCarga := StringToFloatDef(INIRec.ReadString('tot', 'vCarga', ''), 0);
       tot.cUnid  := StrToUnidMed(OK, INIRec.ReadString('tot', 'cUnid', '01'));
       tot.qCarga := StringToFloatDef(INIRec.ReadString('tot', 'qCarga', ''), 0);

       I := 1;
       while true do
       begin
         sSecao := 'lacres' + IntToStrZero(I, 3);
         sFim   := INIRec.ReadString(sSecao, 'nLacre', 'FIM');
         if sFim = 'FIM' then
           break;
         with lacres.Add do
         begin
           nLacre := sFim;
         end;
         Inc(I);
       end;

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

       infAdic.infCpl     := INIRec.ReadString('infAdic', 'infCpl', '');
       infAdic.infAdFisco := INIRec.ReadString('infAdic', 'infAdFisco', '');
      end;
    finally
      INIRec.Free;
    end;
  end;
end;

function GerarMDFeIni( XML : String ) : String;
var
  I, j, y : Integer;
  sSecao : String;
  INIRec : TMemIniFile;
  IniMDFe : TStringList;
  LocMDFeR : TMDFeR;
begin
 INIRec := TMemIniFile.create( 'MDFe.ini' );
 FrmACBrMonitor.ACBrMDFe1.Manifestos.Clear;
 if FilesExists(XML) then
    FrmACBrMonitor.ACBrMDFe1.Manifestos.LoadFromFile(XML)
 else
  begin
    LocMDFeR := TMDFeR.Create(FrmACBrMonitor.ACBrMDFe1.Manifestos.Add.MDFe);
    try
       LocMDFeR.Leitor.Arquivo := ConvertStrRecived( XML );
       LocMDFeR.LerXml;
       FrmACBrMonitor.ACBrMDFe1.Manifestos.Items[0].XML := LocMDFeR.Leitor.Arquivo;
       FrmACBrMonitor.ACBrMDFe1.Manifestos.GerarMDFe;
    finally
       LocMDFeR.Free;
    end;
  end;

 with FrmACBrMonitor do
  begin
   try
      with ACBrMDFe1.Manifestos.Items[0].MDFe do
       begin
         INIRec.WriteInteger('ide', 'cUF', Ide.cUF);
         INIRec.WriteString( 'ide', 'tpEmit', TpEmitenteToStr(Ide.tpEmit));
         INIRec.WriteString( 'ide', 'mod', Ide.modelo);
         INIRec.WriteInteger('ide', 'serie', Ide.serie);
         INIRec.WriteInteger('ide', 'nMDF', Ide.nMDF);
         INIRec.WriteInteger('ide', 'cMDF', Ide.cMDF);
         INIRec.WriteString( 'ide', 'modal', ModalToStr(Ide.modal));
         INIRec.WriteString( 'ide', 'dhEmi', DateToStr(Ide.dhEmi));
         INIRec.WriteString( 'ide', 'tpEmis', TpEmisToStr(Ide.tpEmis));
         INIRec.WriteString( 'ide', 'procEmi', procEmiToStr(Ide.procEmi));
         INIRec.WriteString( 'ide', 'verProc', Ide.verProc);
         INIRec.WriteString( 'ide', 'UFIni', Ide.UFIni);
         INIRec.WriteString( 'ide', 'UFFim', Ide.UFFim);
         INIRec.WriteString( 'ide', 'dhIniViagem', DateToStr(Ide.dhIniViagem));
         INIRec.WriteString( 'ide', 'tpTransp', TTransportadorToStr(Ide.tpTransp));


         for i := 0 to ide.infMunCarrega.Count -1 do
         begin
           sSecao := 'CARR' + IntToStrZero(I+1, 3);

           with ide.infMunCarrega.Items[i] do
           begin
             INIRec.WriteInteger(sSecao, 'cMunCarrega', cMunCarrega);
             INIRec.WriteString(sSecao, 'xMunCarrega', xMunCarrega);
           end;
         end;

         for i := 0 to ide.infPercurso.Count -1 do
         begin
           sSecao := 'PERC' + IntToStrZero(I+1, 3);

           with ide.infPercurso.Items[i] do
           begin
             INIRec.WriteString(sSecao, 'UFPer', UFPer);
           end;
         end;

         INIRec.WriteString('emit', 'CNPJ', Emit.CNPJ);
         INIRec.WriteString('emit', 'IE', Emit.IE);
         INIRec.WriteString('emit', 'xNome', Emit.xNome);
         INIRec.WriteString('emit', 'xFant', Emit.xFant);

         INIRec.WriteString( 'emit', 'xLgr', Emit.enderEmit.xLgr);
         INIRec.WriteString( 'emit', 'nro', Emit.enderEmit.nro);
         INIRec.WriteString( 'emit', 'xCpl', Emit.enderEmit.xCpl);
         INIRec.WriteString( 'emit', 'xBairro', Emit.enderEmit.xBairro);
         INIRec.WriteInteger('emit', 'cMun', Emit.enderEmit.cMun);
         INIRec.WriteString( 'emit', 'xMun', Emit.enderEmit.xMun);
         INIRec.WriteInteger('emit', 'CEP', Emit.enderEmit.CEP);
         INIRec.WriteString( 'emit', 'UF', Emit.enderEmit.UF);
         INIRec.WriteString( 'emit', 'fone', Emit.enderEmit.fone);
         INIRec.WriteString( 'emit', 'email', Emit.enderEmit.email);

         case Ide.modal of
           moRodoviario:  begin
                            INIRec.WriteString('Rodo', 'RNTRC', Rodo.RNTRC);
                            INIRec.WriteString('Rodo', 'CIOT', Rodo.CIOT);
                            INIRec.WriteString('Rodo', 'tpRod', TpRodadoToStr(Rodo.veicTracao.tpRod));
                            INIRec.WriteString('Rodo', 'tpCar', TpCarroceriaToStr(Rodo.veicTracao.tpCar));
                            INIRec.WriteString('Rodo', 'UF', Rodo.veicTracao.UF);

                            if (Rodo.veicTracao.placa <> '') then
                                begin
                                  INIRec.WriteString('veicTracao', 'clInt', Rodo.veicTracao.cInt);
                                  INIRec.WriteString('veicTracao', 'placa', Rodo.veicTracao.placa);
                                  INIRec.WriteString('veicTracao', 'RENAVAN', Rodo.veicTracao.RENAVAM);
                                  INIRec.WriteInteger('veicTracao', 'tara', Rodo.veicTracao.tara);
                                  INIRec.WriteInteger('veicTracao', 'capKG', Rodo.veicTracao.capKG);
                                  INIRec.WriteInteger('veicTracao', 'clInt', Rodo.veicTracao.capM3);
                                end;

                              if ( Rodo.veicTracao.prop.CNPJCPF <> '') then
                              begin
                                INIRec.WriteString('prop','CPFCNPJ',Rodo.veicTracao.prop.CNPJCPF);
                                INIRec.WriteString('prop','RNTRC',Rodo.veicTracao.prop.RNTRC);
                                INIRec.WriteString('prop','xNome',Rodo.veicTracao.prop.xNome);
                                INIRec.WriteString('prop','IE',Rodo.veicTracao.prop.IE);
                                INIRec.WriteString('prop','UF',Rodo.veicTracao.prop.UF);
                                INIRec.WriteString('prop','tpProp',TpPropToStr(Rodo.veicTracao.prop.tpProp));
                                end;

                              for y := 1 to Rodo.veicTracao.condutor.Count -1 do
                                begin
                                sSecao := 'condutor'+IntToStrZero(y+1,3);
                                IniRec.WriteString(sSecao,'CPF',Rodo.veicTracao.condutor.Items[y].CPF);
                                IniRec.WriteString(sSecao,'xNome',Rodo.veicTracao.condutor.Items[y].xNome);
                                end;

                          end;
           moAereo:       begin
                            // Implementar
                          end;
           moAquaviario:  begin
                            // Implementar
                          end;
           moFerroviario: begin
                            // Implementar
                          end;
         end;

         for i := 0 to infDoc.infMunDescarga.Count -1 do
         begin
           sSecao := 'DESC' + IntToStrZero(I+1, 3);

           with infDoc.infMunDescarga.Items[i] do
           begin
             INIRec.WriteInteger(sSecao, 'cMunDescarga', cMunDescarga);
             INIRec.WriteString(sSecao, 'xMunDescarga', xMunDescarga);

             for j := 0 to infDoc.infMunDescarga.Items[i].infCTe.Count -1 do
             begin
               sSecao := 'infCTe' + IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3);

               with infDoc.infMunDescarga.Items[i].infCTe.Items[j] do
               begin
                 INIRec.WriteString(sSecao, 'chCTe', chCTe);
                 INIRec.WriteString(sSecao, 'SegCodBarra', SegCodBarra);

                 // Implementar infUnidTransp - Lista
               end;
             end;

             for j := 0 to infDoc.infMunDescarga.Items[i].infNFe.Count -1 do
             begin
               sSecao := 'infNFe' + IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3);

               with infDoc.infMunDescarga.Items[i].infNFe.Items[j] do
               begin
                 INIRec.WriteString(sSecao, 'chNFe', chNFe);
                 INIRec.WriteString(sSecao, 'SegCodBarra', SegCodBarra);

                 // Implementar infUnidTransp - Lista
               end;
             end;

             for j := 0 to infDoc.infMunDescarga.Items[i].infMDFeTransp.Count -1 do
             begin
               sSecao := 'infMDFeTransp' + IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3);

               with infDoc.infMunDescarga.Items[i].infMDFeTransp.Items[j] do
               begin
                 INIRec.WriteString(sSecao, 'chMDFe', chMDFe);

                 // Implementar infUnidTransp - Lista
               end;
             end;

           end;
         end;

         INIRec.WriteInteger('tot', 'qCTe', tot.qCTe);
         INIRec.WriteInteger('tot', 'qNFe', tot.qNFe);
         INIRec.WriteInteger('tot', 'qMDFe', tot.qMDFe);
         INIRec.WriteFloat( 'tot', 'vCarga', tot.vCarga);
         INIRec.WriteString( 'tot', 'cUnid', UnidMedToStr(tot.cUnid));
         INIRec.WriteFloat( 'tot', 'qCarga', tot.qCarga);

          for i := 0 to lacres.Count - 1 do
          begin
            sSecao := 'lacres' + IntToStrZero(I+1, 3);
            with lacres.Items[i] do
            begin
              INIRec.WriteString(sSecao, 'nLacre', nLacre);
            end;
          end;

          for i := 0 to autXML.Count - 1 do
          begin
            sSecao := 'autXML' + IntToStrZero(I+1, 3);
            with autXML.Items[i] do
            begin
//              if CNPJCPF <> '' then
                INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF)
//              else if CPF <> '' then
//                     INIRec.WriteString(sSecao, 'CNPJCPF', CPF);
            end;
          end;

         INIRec.WriteString('infAdic', 'infAdFisco', infAdic.infAdFisco);
         INIRec.WriteString('infAdic', 'infCpl', infAdic.infCpl);
       end;
   finally
      IniMDFe := TStringList.Create;
      INIRec.GetStrings(IniMDFe);
      INIRec.Free;
      Result := StringReplace(IniMDFe.Text,sLineBreak+sLineBreak,sLineBreak,[rfReplaceAll]);
      IniMDFe.Free;
   end;

  end;

end;

end.

