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

unit DoACBrNFeUnit ;

interface

Uses Classes, TypInfo, SysUtils, CmdUnit,  StdCtrls;

Procedure DoACBrNFe( Cmd : TACBrCmd ) ;
function UFparaCodigo(const UF: string): integer;
function SubstituirVariaveis(const ATexto: String): String;

implementation

Uses IniFiles, StrUtils, DateUtils, Forms,
  ACBrUtil, ACBrDFeUtil, ACBrMonitor1 ,
  ACBrNFeDANFEClass, DoACBrUnit,
  pcnNFe, pcnConversao, pcnConversaoNFe,
  pcnAuxiliar, pcnNFeRTXT,  pcnNFeR;

procedure DoACBrNFe(Cmd: TACBrCmd);
var
  I, J, K, nNumCopias : Integer;
  ArqNFe, ArqPDF, ArqEvento, Chave, cImpressora : String;
  Salva, OK, bImprimir, bImprimirPDF : Boolean;
  bMostrarPreview : String;
  ChavesNFe, PathsNFe: Tstrings;
  Alertas : AnsiString;
  wDiretorioAtual : String;

  Lines   : TStringList ;
  sMensagemEmail, SL: TStringList;
  MemoTXT : TMemo;
  Files  : String ;
  dtFim  : TDateTime ;

  RetFind   : Integer ;
  SearchRec : TSearchRec ;

  NFeRTXT   : TNFeRTXT;
  VersaoDF  : TpcnVersaoDF;
  ModeloDF  : TpcnModeloDF;
  TipoDANFE : TpcnTipoImpressao;

  CC, Anexos: Tstrings;
  sTemMais,ErrosRegraNegocio: String;
  tipoEvento: TpcnTpEvento;
  FormaEmissao: TpcnTipoEmissao;

begin
 with FrmACBrMonitor do
  begin
     wDiretorioAtual := GetCurrentDir;
     try
        //NFe.StatusServico
        if Cmd.Metodo = 'statusservico' then
         begin
           ValidarIntegradorNFCe();
           ACBrNFe1.WebServices.StatusServico.Executar;
           Cmd.Resposta := ACBrNFe1.WebServices.StatusServico.Msg+
                              '[STATUS]'+sLineBreak+
                              'Versao='+ACBrNFe1.WebServices.StatusServico.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.StatusServico.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrNFe1.WebServices.StatusServico.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrNFe1.WebServices.StatusServico.CStat)+sLineBreak+
                              'XMotivo='+ACBrNFe1.WebServices.StatusServico.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrNFe1.WebServices.StatusServico.CUF)+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.StatusServico.DhRecbto)+sLineBreak+
                              'TMed='+IntToStr(ACBrNFe1.WebServices.StatusServico.TMed)+sLineBreak+
                              'DhRetorno='+DateTimeToStr(ACBrNFe1.WebServices.StatusServico.DhRetorno)+sLineBreak+
                              'XObs='+ACBrNFe1.WebServices.StatusServico.XObs+sLineBreak;
         end

        //NFe.ValidarNFe(cArqXML)
        else if Cmd.Metodo = 'validarnfe' then
         begin
           ACBrNFe1.NotasFiscais.Clear;
           CarregarDFe(Cmd.Params(0), ArqNFe);

           ACBrNFe1.NotasFiscais.Validar;
         end

       //NFe.ValidarNfeRegraNegocios(cArqXML)
       else if Cmd.Metodo = 'validarnferegranegocios' then
        begin
          ACBrNFe1.NotasFiscais.Clear;
          CarregarDFe(Cmd.Params(0), ArqNFe);
          ACBrNFe1.NotasFiscais.ValidarRegrasdeNegocios(ErrosRegraNegocio);

          if NaoEstaVazio(ErrosRegraNegocio) then
            raise Exception.Create(ErrosRegraNegocio);
        end

       //NFe.AssinarNFe(cArqXML)
        else if Cmd.Metodo = 'assinarnfe' then
         begin
           ACBrNFe1.NotasFiscais.Clear;
           CarregarDFe(Cmd.Params(0), ArqNFe);
           Salva := ACBrNFe1.Configuracoes.Geral.Salvar;
           if not Salva then
            begin
             ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
             ACBrNFe1.Configuracoes.Arquivos.PathSalvar := PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs';
            end;

           ACBrNFe1.Configuracoes.Geral.Salvar := True;
           ACBrNFe1.NotasFiscais.Assinar;
           ACBrNFe1.Configuracoes.Geral.Salvar := Salva;

           if NaoEstaVazio(ACBrNFe1.NotasFiscais.Items[0].NomeArq) then
              Cmd.Resposta := ACBrNFe1.NotasFiscais.Items[0].NomeArq
           else
              Cmd.Resposta := PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+StringReplace(ACBrNFe1.NotasFiscais.Items[0].NFe.infNFe.ID, 'NFe', '', [rfIgnoreCase])+'-nfe.xml';
         end

        //NFe.ConsultarNFe(cArqXML)
        else if Cmd.Metodo = 'consultarnfe' then
         begin
           ACBrNFe1.NotasFiscais.Clear;

           PathsNFe := TStringList.Create;
           try
             PathsNFe.Append(Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)+'-nfe.xml');
             CarregarDFe(PathsNFe, ArqNFe);

           finally
             PathsNFe.Free;
           end;

           if ACBrNFe1.NotasFiscais.Count = 0 then
           begin
             if ValidarChave(Cmd.Params(0)) then
               ACBrNFe1.WebServices.Consulta.NFeChave := Cmd.Params(0)
             else
               raise Exception.Create('Parâmetro incorreto. Chave inválida ou arquivo não encontrado.');
           end
           else
             ACBrNFe1.WebServices.Consulta.NFeChave := StringReplace(ACBrNFe1.NotasFiscais.Items[0].NFe.infNFe.ID,'NFe','',[rfIgnoreCase]);

           ValidarIntegradorNFCe(ACBrNFe1.WebServices.Consulta.NFeChave);
           try
              ACBrNFe1.WebServices.Consulta.Executar;

              Cmd.Resposta := ACBrNFe1.WebServices.Consulta.Msg+sLineBreak+
                              '[CONSULTA]'+sLineBreak+
                              'Versao='+ACBrNFe1.WebServices.Consulta.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Consulta.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrNFe1.WebServices.Consulta.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrNFe1.WebServices.Consulta.CStat)+sLineBreak+
                              'XMotivo='+ACBrNFe1.WebServices.Consulta.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrNFe1.WebServices.Consulta.CUF)+sLineBreak+
                              'ChNFe='+ACBrNFe1.WebServices.Consulta.NFeChave+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.Consulta.DhRecbto)+sLineBreak+
                              'NProt='+ACBrNFe1.WebServices.Consulta.Protocolo+sLineBreak+
                              'DigVal='+ACBrNFe1.WebServices.Consulta.protNFe.digVal+sLineBreak+
                              IfThen(EstaVazio(ArqNFe),'','Arquivo='+ArqNFe+sLineBreak);


              if NaoEstaVazio(Trim(ACBrNFe1.WebServices.Consulta.retCancNFe.nProt)) then
              begin
                Cmd.Resposta := Cmd.Resposta +
                              '[INFCANC]'+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Consulta.retCancNFe.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrNFe1.WebServices.Consulta.retCancNFe.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrNFe1.WebServices.Consulta.retCancNFe.CStat)+sLineBreak+
                              'XMotivo='+ACBrNFe1.WebServices.Consulta.retCancNFe.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrNFe1.WebServices.Consulta.retCancNFe.CUF)+sLineBreak+
                              'ChNFe='+ACBrNFe1.WebServices.Consulta.retCancNFe.chNFE+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.Consulta.retCancNFe.DhRecbto)+sLineBreak+
                              'NProt='+ACBrNFe1.WebServices.Consulta.retCancNFe.nProt+sLineBreak;
              end;

              for I:= 0 to ACBrNFe1.WebServices.Consulta.procEventoNFe.Count-1 do
              begin
                Cmd.Resposta := Cmd.Resposta +
                              '[PROCEVENTONFE'+IntToStrZero(I+1,3)+']'+sLineBreak+
                              'ID='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].ID)+sLineBreak+
                              'cOrgao='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.cOrgao)+sLineBreak+
                              'tpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.tpAmb)+sLineBreak+
                              'CNPJ='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.CNPJ+sLineBreak+
                              'chNFe='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.chNFe+sLineBreak+
                              'dhEvento='+DateTimeToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.dhEvento)+sLineBreak+
                              'tpEvento='+TpEventoToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.tpEvento)+sLineBreak+
                              'nSeqEvento='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.nSeqEvento)+sLineBreak+
                              'verEvento='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.versaoEvento+sLineBreak+
                              '[detEvento'+IntToStrZero(I+1,3)+']'+sLineBreak+
                              'versao='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.versao+sLineBreak+
                              'descEvento='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.descEvento+sLineBreak+
                              'xCorrecao='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.xCorrecao+sLineBreak+
                              'xCondUso='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.xCondUso+sLineBreak+
                              'nProt='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.nProt+sLineBreak+
                              'xJust='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.xJust+sLineBreak+
                              'cOrgaoAutor='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.cOrgaoAutor)+sLineBreak+
                              'tpAutor='+TipoAutorToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.tpAutor)+sLineBreak+
                              'verAplic='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.verAplic+sLineBreak+
                              'dhEmi='+DateTimeToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.dhEmi)+sLineBreak+
                              'tpNF='+tpNFToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.tpNF)+sLineBreak+
                              'IE='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.IE+sLineBreak+
                              'DESTCNPJCPF='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.dest.CNPJCPF+sLineBreak+
                              'DESTidEstrangeiro='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.dest.idEstrangeiro+sLineBreak+
                              'DESTIE='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.dest.IE+sLineBreak+
                              'DESTUF='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.dest.UF+sLineBreak+
                              'vNF='+FloatToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.vNF)+sLineBreak+
                              'vICMS='+FloatToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.vICMS)+sLineBreak+
                              'vST='+FloatToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.vST)+sLineBreak+
                              'idPedidoCancelado='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.idPedidoCancelado+sLineBreak;
                              for J:= 0 to ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.itemPedido.Count-1 do
                              begin
                                Cmd.Resposta := Cmd.Resposta +
                                '[itemPedido'+IntToStrZero(I+1,3)+IntToStrZero(J+1,3)+']'+sLineBreak+
                                'numItem='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.itemPedido.Items[J].numItem)+sLineBreak+
                                'qtdeItem='+FloatToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.InfEvento.detEvento.itemPedido.Items[J].qtdeItem)+sLineBreak;
                              end;

                              for J:= 0 to ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Count-1 do
                              begin
                                Cmd.Resposta := Cmd.Resposta +
                                '[RETEVENTO'+IntToStrZero(I+1,3)+IntToStrZero(J+1,3)+']'+sLineBreak+
                                'Id='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].ID)+sLineBreak+
                                'NomeArquivo='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.NomeArquivo+sLineBreak+
                                'tpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.tpAmb)+sLineBreak+
                                'verAplic='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.verAplic+sLineBreak+
                                'cOrgao='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.cOrgao)+sLineBreak+
                                'cStat='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.cStat)+sLineBreak+
                                'xMotivo='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.xMotivo+sLineBreak+
                                'chNFe='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.chNFe+sLineBreak+
                                'tpEvento='+TpEventoToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.tpEvento)+sLineBreak+
                                'xEvento='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.xEvento+sLineBreak+
                                'nSeqEvento='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.nSeqEvento)+sLineBreak+
                                'CNPJDest='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.CNPJDest+sLineBreak+
                                'emailDest='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.emailDest+sLineBreak+
                                'cOrgaoAutor='+IntToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.cOrgaoAutor)+sLineBreak+
                                'dhRegEvento='+DateTimeToStr(ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.dhRegEvento)+sLineBreak+
                                'nProt='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.nProt+sLineBreak+
                                'XML='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.XML+sLineBreak;

                                for K:=0 to ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.chNFePend.Count-1 do
                                begin
                                 Cmd.Resposta := Cmd.Resposta +
                                '[chNFePend'+IntToStrZero(I+1,3)+IntToStrZero(J+1,3)+IntToStrZero(K+1,3)+']'+sLineBreak+
                                'chNFePend='+ACBrNFe1.WebServices.Consulta.procEventoNFe.Items[i].RetEventoNFe.retEvento.Items[J].RetInfEvento.chNFePend.Items[K].ChavePend+sLineBreak;
                                end;
                              end;
              end;
           except
             on E: Exception do
             begin
               raise Exception.Create(ACBrNFe1.WebServices.Consulta.Msg+sLineBreak+E.Message);
             end;
           end;
         end

        else if Cmd.Metodo = 'cancelarnfe' then  //NFe.CancelarNFe(cChaveNFe,cJustificativa,cCNPJ,nEvento)
         begin
           ACBrNFe1.NotasFiscais.Clear;
           if not ValidarChave(Cmd.Params(0)) then
              raise Exception.Create('Chave '+Cmd.Params(0)+' inválida.')
           else
              ACBrNFe1.WebServices.Consulta.NFeChave := Cmd.Params(0);

           if not ACBrNFe1.WebServices.Consulta.Executar then
              raise Exception.Create(ACBrNFe1.WebServices.Consulta.Msg);

           ACBrNFe1.EventoNFe.Evento.Clear;
           with ACBrNFe1.EventoNFe.Evento.Add do
            begin
              infEvento.CNPJ   := Cmd.Params(2);
              if Trim(infEvento.CNPJ) = '' then
                 infEvento.CNPJ   := copy(OnlyNumber(ACBrNFe1.WebServices.Consulta.NFeChave),7,14)
              else
              begin
                if not ValidarCNPJ(Cmd.Params(2)) then
                  raise Exception.Create('CNPJ '+Cmd.Params(2)+' inválido.')
              end;

              infEvento.cOrgao := StrToIntDef(copy(OnlyNumber(ACBrNFe1.WebServices.Consulta.NFeChave),1,2),0);
              infEvento.dhEvento := now;
              infEvento.tpEvento := teCancelamento;
              infEvento.chNFe := ACBrNFe1.WebServices.Consulta.NFeChave;
              infEvento.detEvento.nProt := ACBrNFe1.WebServices.Consulta.Protocolo;
              infEvento.detEvento.xJust := Cmd.Params(1);
            end;

           ValidarIntegradorNFCe(Cmd.Params(0));
           try
              ACBrNFe1.EnviarEvento(StrToIntDef(Cmd.Params(3),1));

              Cmd.Resposta := ACBrNFe1.WebServices.EnvEvento.EventoRetorno.xMotivo+sLineBreak+
                              '[CANCELAMENTO]'+sLineBreak+
                              'Versao='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.verAplic+sLineBreak+
                              'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.TpAmb)+sLineBreak+
                              'VerAplic='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.VerAplic+sLineBreak+
                              'CStat='+IntToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat)+sLineBreak+
                              'XMotivo='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.XMotivo+sLineBreak+
                              'CUF='+IntToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cOrgao)+sLineBreak+
                              'ChNFe='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.chNFe+sLineBreak+
                              'DhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento)+sLineBreak+
                              'NProt='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt+sLineBreak+
                              'tpEvento='+TpEventoToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.tpEvento)+sLineBreak+
                              'xEvento='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xEvento+sLineBreak+
                              'nSeqEvento='+IntToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nSeqEvento)+sLineBreak+
                              'CNPJDest='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.CNPJDest+sLineBreak+
                              'emailDest='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.emailDest+sLineBreak+
                              'Arquivo='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.NomeArquivo+sLineBreak+
                              'XML='+ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.XML+sLineBreak;
           except
             on E: Exception do
              begin
                raise Exception.Create(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.xMotivo+sLineBreak+E.Message);
              end;
           end;
         end
        //NFe.ImprimirDanfe(cArqXML,cImpressora,nNumCopias,cProtocolo,bMostrarPreview,cMarcaDaqgua,bViaConsumidor,bSimplificado)
        else if Cmd.Metodo = 'imprimirdanfe' then
         begin
           ACBrNFe1.NotasFiscais.Clear;

           PathsNFe := TStringList.Create;
           try
             PathsNFe.Append(Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)+'-nfe.xml');
             CarregarDFe(PathsNFe, ArqNFe);

           finally
             PathsNFe.Free;
           end;

           bMostrarPreview := Cmd.Params(4);
           ConfiguraDANFe(False, bMostrarPreview );

           if NaoEstaVazio(Cmd.Params(1)) then
              ACBrNFe1.DANFE.Impressora := Cmd.Params(1);

           if NaoEstaVazio(Cmd.Params(2)) then
              ACBrNFe1.DANFE.NumCopias := StrToIntDef(Cmd.Params(2),1);

           if NaoEstaVazio(Cmd.Params(3)) then
              ACBrNFe1.DANFE.ProtocoloNFe := Cmd.Params(3)
           else
             ACBrNFe1.DANFE.ProtocoloNFe:= '';

           if NaoEstaVazio(Cmd.Params(5)) then
            begin
              ACBrNFeDANFeRL1.MarcadAgua := Cmd.Params(5);
            end
           else
            begin
              ACBrNFeDANFeRL1.MarcadAgua := '';
            end ;

           if NaoEstaVazio(Cmd.Params(6)) then
              ACBrNFe1.DANFE.ViaConsumidor := (Cmd.Params(6) = '1');

           if NaoEstaVazio(Cmd.Params(7)) then
            begin
              if (Cmd.Params(7) = '1') then
                ACBrNFe1.DANFE.TipoDANFE := tiSimplificado;
            end;

           try
             AntesDeImprimir( ( StrToBoolDef(bMostrarPreview, False) ) or
                              (FrmACBrMonitor.cbxMostrarPreview.Checked)  );
             ACBrNFe1.NotasFiscais.Imprimir;
           finally
             DepoisDeImprimir;
           end;

           Cmd.Resposta := 'Danfe Impresso com sucesso';
         end

        else if Cmd.Metodo = 'imprimirdanfepdf' then //NFe.ImprimirDANFEPDF(cArqXML,cProtocolo,cMarcaDaqgua,bViaConsumidor,bSimplificado)
         begin
           ACBrNFe1.NotasFiscais.Clear;

           CarregarDFe(Cmd.Params(0), ArqNFe);
           ConfiguraDANFe(True, '');

           if NaoEstaVazio(Cmd.Params(1)) then
              ACBrNFe1.DANFE.ProtocoloNFe := Cmd.Params(1)
           else
             ACBrNFe1.DANFE.ProtocoloNFe:= '';

           if NaoEstaVazio(Cmd.Params(2)) then
              ACBrNFeDANFeRL1.MarcadAgua := Cmd.Params(2)
           else
              ACBrNFeDANFeRL1.MarcadAgua := '';

           if NaoEstaVazio(Cmd.Params(3)) then
              ACBrNFe1.DANFE.ViaConsumidor := (Cmd.Params(3) = '1');

           if NaoEstaVazio(Cmd.Params(4)) then
            begin
              if (Cmd.Params(4) = '1') then
                ACBrNFe1.DANFE.TipoDANFE := tiSimplificado;
            end;

           try
             try
               AntesDeImprimir(False);
               ACBrNFe1.NotasFiscais.ImprimirPDF;
             finally
               DepoisDeImprimir;
             end;

              ArqPDF := OnlyNumber(ACBrNFe1.NotasFiscais.Items[0].NFe.infNFe.ID)+'-nfe.pdf';

              Cmd.Resposta := 'Arquivo criado em: '+ PathWithDelim(ACBrNFe1.DANFE.PathPDF) + ArqPDF ;
           except
              raise Exception.Create('Erro ao criar o arquivo PDF');
           end;
         end

        //NFe.ImprimirEvento(cPathXMLEvento,cPathXMLNFe,cImpressora,nNumCopias,bMostrarPreview)
        //NFe.ImprimirEventoPDF(cPathXMLEvento,cPathXMLNFe)
        else if ( Cmd.Metodo = 'imprimirevento' ) or ( Cmd.Metodo = 'imprimireventopdf' ) then
         begin
           ACBrNFe1.EventoNFe.Evento.Clear;

           PathsNFe := TStringList.Create;
           try
             PathsNFe.Append(Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)+'-eve.xml');
             try
               CarregarDFe(PathsNFe, ArqEvento, tDFeEventoNFe);
             except
                on E: Exception do
                begin
                   raise Exception.Create('Erro ao Carregar DFe: '+E.Message);
                end;
             end;
           finally
             PathsNFe.Free;
           end;

           ACBrNFe1.NotasFiscais.Clear;
           if NaoEstaVazio(Cmd.Params(1)) then
           begin
             PathsNFe := TStringList.Create;
             try
               PathsNFe.Append(Cmd.Params(1));
               PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
               try
                 CarregarDFe(PathsNFe, ArqNFe);
               except
                  on E: Exception do
                  begin
                    raise Exception.Create('Erro ao Carregar DFe: '+E.Message);
                  end;
               end;
             finally
               PathsNFe.Free;
             end;
           end;

           bMostrarPreview :=  BoolToStr((Cmd.Metodo = 'imprimirevento' ) and (Cmd.Params(4) = '1'));
           ConfiguraDANFe(False, bMostrarPreview );
           if (rgModoImpressaoEvento.ItemIndex = 0) or (ACBrNFe1.DANFE = ACBrNFeDANFCeFortes1) then  //Atualmente não existe impressão de eventos em Fortes para Bobina
              ACBrNFe1.DANFE := ACBrNFeDANFeRL1;

           if ( Cmd.Metodo = 'imprimirevento' ) then
           begin
             if NaoEstaVazio(Cmd.Params(2)) then
               ACBrNFe1.DANFE.Impressora := Cmd.Params(2)
             else
             begin
               if rgModoImpressaoEvento.ItemIndex = 0 then
                 ACBrNFe1.DANFE.Impressora := cbxImpressora.Text
               else
                 ACBrNFe1.DANFE.Impressora := cbxImpressoraNFCe.Text;
             end;

             if NaoEstaVazio(Cmd.Params(3)) then
               ACBrNFe1.DANFE.NumCopias := StrToIntDef(Cmd.Params(3),1);

             try
               AntesDeImprimir( ( StrToBoolDef(bMostrarPreview, False) ) or
                              (FrmACBrMonitor.cbxMostrarPreview.Checked) );
               ACBrNFe1.ImprimirEvento;
             finally
               DepoisDeImprimir;
             end;
             Cmd.Resposta := 'Evento Impresso com sucesso';
           end
           else
           begin
             try
               ACBrNFe1.ImprimirEventoPDF;
               ArqPDF := OnlyNumber(ACBrNFe1.EventoNFe.Evento[0].InfEvento.id);
               ArqPDF := PathWithDelim(ACBrNFe1.DANFE.PathPDF)+ArqPDF+'-procEventoNFe.pdf';
               Cmd.Resposta := 'Arquivo criado em: ' + ArqPDF ;
             except
               raise Exception.Create('Erro ao criar o arquivo PDF');
             end;
           end;
         end

        //NFe.InutilizarNFe(cCNPJ,cJustificativa,nAno,nModelo,nSérie,nNumInicial,nNumFinal)
        else if Cmd.Metodo = 'inutilizarnfe' then
        begin
          ValidarIntegradorNFCe( IntToStrZero(0,20) + Cmd.Params(3));
                                         //CNPJ         //Justificat   //Ano                    //Modelo                 //Série                  //Num.Inicial            //Num.Final
          ACBrNFe1.WebServices.Inutiliza(Cmd.Params(0), Cmd.Params(1), StrToInt(Cmd.Params(2)), StrToInt(Cmd.Params(3)), StrToInt(Cmd.Params(4)), StrToInt(Cmd.Params(5)), StrToInt(Cmd.Params(6)));
                   Cmd.Resposta := ACBrNFe1.WebServices.Inutilizacao.Msg+sLineBreak+
                          '[INUTILIZACAO]'+sLineBreak+
                          'Versao='+ACBrNFe1.WebServices.Inutilizacao.verAplic+sLineBreak+
                          'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Inutilizacao.TpAmb)+sLineBreak+
                          'VerAplic='+ACBrNFe1.WebServices.Inutilizacao.VerAplic+sLineBreak+
                          'CStat='+IntToStr(ACBrNFe1.WebServices.Inutilizacao.CStat)+sLineBreak+
                          'XMotivo='+ACBrNFe1.WebServices.Inutilizacao.XMotivo+sLineBreak+
                          'CUF='+IntToStr(ACBrNFe1.WebServices.Inutilizacao.CUF)+sLineBreak+
                          'DhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.Inutilizacao.DhRecbto)+sLineBreak+
                          'NProt='+ACBrNFe1.WebServices.Inutilizacao.Protocolo+sLineBreak+
                          'Arquivo='+ACBrNFe1.WebServices.Inutilizacao.NomeArquivo+sLineBreak+
                          'XML='+ACBrNFe1.WebServices.Inutilizacao.XML_ProcInutNFe+sLineBreak;
        end
        else if ( Cmd.Metodo = 'imprimirinutilizacao' )  then
        begin
          PathsNFe := TStringList.Create;
          try
             PathsNFe.Append(Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)+'-inu.xml');
             CarregarDFe(PathsNFe, ArqEvento, tDFeInutNFe);

          finally
            PathsNFe.Free;
          end;

          bMostrarPreview := BoolToStr((Cmd.Metodo = 'imprimirinutilizacao' ) and (Cmd.Params(3) = '1'));
          ConfiguraDANFe(False, bMostrarPreview );
          ACBrNFe1.DANFE := ACBrNFeDANFeRL1;

          if NaoEstaVazio(Cmd.Params(1)) then
            ACBrNFe1.DANFE.Impressora := Cmd.Params(1)
          else
          begin
            if rgModoImpressaoEvento.ItemIndex = 0 then
              ACBrNFe1.DANFE.Impressora := cbxImpressora.Text
            else
              ACBrNFe1.DANFE.Impressora := cbxImpressoraNFCe.Text;
          end;

          if NaoEstaVazio(Cmd.Params(2)) then
            ACBrNFe1.DANFE.NumCopias := StrToIntDef(Cmd.Params(2),1);

          try
            AntesDeImprimir( ( StrToBoolDef(bMostrarPreview, False) ) or
                           (FrmACBrMonitor.cbxMostrarPreview.Checked) );
            ACBrNFe1.ImprimirInutilizacao;
          finally
            DepoisDeImprimir;
          end;
          Cmd.Resposta := 'Inutilização Impressa com sucesso';
        end

        else if ( Cmd.Metodo = 'imprimirinutilizacaopdf' ) then
        begin
          PathsNFe := TStringList.Create;
          try
             PathsNFe.Append(Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)+'-inu.xml');
             try
               CarregarDFe(PathsNFe, ArqEvento, tDFeInutNFe);
             except
               on E: Exception do
               begin
                 raise Exception.Create('Erro ao Carregar XML Inutilização: '+E.Message);
               end;
             end;
          finally
            PathsNFe.Free;
          end;

          ACBrNFe1.NotasFiscais.Clear;
          if NaoEstaVazio(Cmd.Params(1)) then
          begin
            PathsNFe := TStringList.Create;
            try
              PathsNFe.Append(Cmd.Params(1));
              PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
              try
                CarregarDFe(PathsNFe, ArqNFe);
              except
                 on E: Exception do
                 begin
                   raise Exception.Create('Erro ao Carregar DFe: '+E.Message);
                 end;
              end;
            finally
              PathsNFe.Free;
            end;
          end;

          ConfiguraDANFe(False, '' );
          ACBrNFe1.DANFE := ACBrNFeDANFeRL1;

          try
            ACBrNFe1.ImprimirInutilizacaoPDF;
            ArqPDF := OnlyNumber(ACBrNFe1.InutNFe.ID);
            ArqPDF := PathWithDelim(ACBrNFe1.DANFE.PathPDF)+ArqPDF+'-procInutNFe.pdf';
            Cmd.Resposta := 'Arquivo criado em: ' + ArqPDF ;
          except
            raise Exception.Create('Erro ao criar o arquivo PDF');
          end;

        end

        //NFe.EnviarNFe(cArqXML,nLote,[bAssina],[bImprime],[cImpressora],[bSincrono])
        else if Cmd.Metodo = 'enviarnfe' then
         begin
           ACBrNFe1.NotasFiscais.Clear;
           CarregarDFe(Cmd.Params(0), ArqNFe);
           ACBrNFe1.NotasFiscais.GerarNFe;

           if Cmd.Params(2) <> '0' then
              ACBrNFe1.NotasFiscais.Assinar;

           ACBrNFe1.NotasFiscais.Validar;

           if Trim(OnlyNumber(Cmd.Params(1))) = '' then
              ACBrNFe1.WebServices.Enviar.Lote := '1'
           else
              ACBrNFe1.WebServices.Enviar.Lote := OnlyNumber(Cmd.Params(1));

           ValidarIntegradorNFCe(ACBrNFe1.NotasFiscais.Items[0].NFe.infNFe.ID  );
           ACBrNFe1.WebServices.Enviar.Sincrono := (Cmd.Params(5)='1');
           ACBrNFe1.WebServices.Enviar.Executar;

           Cmd.Resposta :=  ACBrNFe1.WebServices.Enviar.Msg+sLineBreak+
                            '[ENVIO]'+sLineBreak+
                            'Versao='+ACBrNFe1.WebServices.Enviar.verAplic+sLineBreak+
                            'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Enviar.TpAmb)+sLineBreak+
                            'VerAplic='+ACBrNFe1.WebServices.Enviar.VerAplic+sLineBreak+
                            'CStat='+IntToStr(ACBrNFe1.WebServices.Enviar.CStat)+sLineBreak+
                            'XMotivo='+ACBrNFe1.WebServices.Enviar.XMotivo+sLineBreak+
                            'CUF='+IntToStr(ACBrNFe1.WebServices.Enviar.CUF)+sLineBreak+
                            'NRec='+ACBrNFe1.WebServices.Enviar.Recibo+sLineBreak+
                            'DhRecbto='+DateTimeToStr( ACBrNFe1.WebServices.Enviar.dhRecbto)+sLineBreak+
                            'TMed='+IntToStr( ACBrNFe1.WebServices.Enviar.tMed)+sLineBreak+
                            'Recibo='+ACBrNFe1.WebServices.Enviar.Recibo+sLineBreak;

           if ACBrNFe1.WebServices.Enviar.Recibo <> '' then
            begin
              ACBrNFe1.WebServices.Retorno.Recibo := ACBrNFe1.WebServices.Enviar.Recibo;
              ACBrNFe1.WebServices.Retorno.Executar;

              Cmd.Resposta :=  Cmd.Resposta+
                               ACBrNFe1.WebServices.Retorno.Msg+sLineBreak+
                               '[RETORNO]'+sLineBreak+
                               'Versao='+ACBrNFe1.WebServices.Retorno.verAplic+sLineBreak+
                               'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Retorno.TpAmb)+sLineBreak+
                               'VerAplic='+ACBrNFe1.WebServices.Retorno.VerAplic+sLineBreak+
                               'NRec='+ACBrNFe1.WebServices.Retorno.NFeRetorno.nRec+sLineBreak+
                               'CStat='+IntToStr(ACBrNFe1.WebServices.Retorno.CStat)+sLineBreak+
                               'XMotivo='+ACBrNFe1.WebServices.Retorno.XMotivo+sLineBreak+
                               'CUF='+IntToStr(ACBrNFe1.WebServices.Retorno.CUF)+sLineBreak;

              for I:= 0 to ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Count-1 do
               begin
                 for J:= 0 to ACBrNFe1.NotasFiscais.Count-1 do
                 begin
                   if 'NFe'+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].chNFe = ACBrNFe1.NotasFiscais.Items[j].NFe.InfNFe.Id  then
                   begin
                     Cmd.Resposta := Cmd.Resposta+
                                '[NFE'+Trim(IntToStr(ACBrNFe1.NotasFiscais.Items[J].NFe.Ide.NNF))+']'+sLineBreak+
                                'Versao='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].verAplic+sLineBreak+
                                'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].tpAmb)+sLineBreak+
                                'VerAplic='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].verAplic+sLineBreak+
                                'CStat='+IntToStr(ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].cStat)+sLineBreak+
                                'XMotivo='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].xMotivo+sLineBreak+
                                'CUF='+IntToStr(ACBrNFe1.WebServices.Retorno.NFeRetorno.cUF)+sLineBreak+
                                'ChNFe='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].chNFe+sLineBreak+
                                'DhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].dhRecbto)+sLineBreak+
                                'NProt='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].nProt+sLineBreak+
                                'DigVal='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].digVal+sLineBreak;
                     break;
                   end;
                 end;
                if ACBrNFe1.NotasFiscais.Items[0].Confirmada and (Cmd.Params(3) = '1') then
                 begin
                   ConfiguraDANFe(False, '');

                   if NaoEstaVazio(Cmd.Params(4)) then
                     ACBrNFe1.DANFE.Impressora := Cmd.Params(4);

                   try
                     AntesDeImprimir(False);
                     ACBrNFe1.NotasFiscais.Items[0].Imprimir;
                   finally
                     DepoisDeImprimir;
                   end;
                 end;
               end;
            end
           else
            begin
              if ACBrNFe1.NotasFiscais.Items[0].Confirmada and (Cmd.Params(3) = '1') then
               begin
                 ConfiguraDANFe(False, '');

                 if NaoEstaVazio(Cmd.Params(4)) then
                    ACBrNFe1.DANFE.Impressora := Cmd.Params(4);

                 try
                   AntesDeImprimir(False);
                   ACBrNFe1.NotasFiscais.Items[0].Imprimir;
                 finally
                   DepoisDeImprimir;
                 end;
               end;
            end;
         end

        //NFe.ReciboNFe(nRecibo)
        else if (Cmd.Metodo = 'recibonfe')then
         begin
           ACBrNFe1.WebServices.Recibo.Recibo := Cmd.Params(0);

           ValidarIntegradorNFCe();
           if not(ACBrNFe1.WebServices.Recibo.Executar) then
             raise Exception.Create(ACBrNFe1.WebServices.Recibo.xMotivo);

           Cmd.Resposta :=  Cmd.Resposta+
                            ACBrNFe1.WebServices.Recibo.Msg+sLineBreak+
                           '[RETORNO]'+sLineBreak+
                           'Versao='+ACBrNFe1.WebServices.Recibo.verAplic+sLineBreak+
                           'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Recibo.TpAmb)+sLineBreak+
                           'VerAplic='+ACBrNFe1.WebServices.Recibo.VerAplic+sLineBreak+
                           'NRec='+ACBrNFe1.WebServices.Recibo.Recibo+sLineBreak+
                           'CStat='+IntToStr(ACBrNFe1.WebServices.Recibo.CStat)+sLineBreak+
                           'XMotivo='+ACBrNFe1.WebServices.Recibo.XMotivo+sLineBreak+
                           'CUF='+IntToStr(ACBrNFe1.WebServices.Recibo.CUF)+sLineBreak+
                           'ChNFe='+ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[0].chNFe+sLineBreak+
                           'NProt='+ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[0].nProt+sLineBreak+
                           'MotivoNFe='+ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[0].xMotivo+sLineBreak;

                           for I:= 0 to ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Count-1 do
                            begin
                              Cmd.Resposta := Cmd.Resposta+
                                '[NFE'+Trim(IntToStr(StrToInt(copy(ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].chNFe,26,9))))+']'+sLineBreak+
                                'Versao='+ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].verAplic+sLineBreak+
                                'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].tpAmb)+sLineBreak+
                                'VerAplic='+ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].verAplic+sLineBreak+
                                'CStat='+IntToStr(ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].cStat)+sLineBreak+
                                'XMotivo='+ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].xMotivo+sLineBreak+
                                'CUF='+IntToStr(ACBrNFe1.WebServices.Recibo.NFeRetorno.cUF)+sLineBreak+
                                'ChNFe='+ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].chNFe+sLineBreak+
                                'DhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].dhRecbto)+sLineBreak+
                                'NProt='+ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].nProt+sLineBreak+
                                'DigVal='+ACBrNFe1.WebServices.Recibo.NFeRetorno.ProtNFe.Items[i].digVal+sLineBreak;
                            end;

           if ACBrNFe1.Configuracoes.Geral.Salvar then
            begin
              Cmd.Resposta :=  Cmd.Resposta+
              'Arquivo='+ACBrNFe1.Configuracoes.Arquivos.PathSalvar+Cmd.Params(0)+'-pro-rec.xml';
            end;
         end

        //NFe.ConsultaCadastro(cUF,nDocumento,[nIE])
        else if (Cmd.Metodo = 'consultacadastro')then
         begin
           ACBrNFe1.WebServices.ConsultaCadastro.UF   := Cmd.Params(0);
           if (Cmd.Params(2) = '1') then
              ACBrNFe1.WebServices.ConsultaCadastro.IE := Cmd.Params(1)
           else
            begin
              if Length(Cmd.Params(1)) > 11 then
                 ACBrNFe1.WebServices.ConsultaCadastro.CNPJ := Cmd.Params(1)
              else
                 ACBrNFe1.WebServices.ConsultaCadastro.CPF := Cmd.Params(1);
            end;

            ValidarIntegradorNFCe();
            ACBrNFe1.WebServices.ConsultaCadastro.Executar;

            Cmd.Resposta :=  Cmd.Resposta+
                             '[CONSULTACADASTRO]'+sLineBreak+
                             ACBrNFe1.WebServices.ConsultaCadastro.Msg+sLineBreak+
                             'verAplic=' +ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.verAplic+sLineBreak+
                             'cStat='    +IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.cStat)+sLineBreak+
                             'xMotivo='  +ACBrNFe1.WebServices.ConsultaCadastro.xMotivo+sLineBreak+
                             'UF='       +ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.UF+sLineBreak+
                             'IE='       +ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.IE+sLineBreak+
                             'CNPJ='     +ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.CNPJ+sLineBreak+
                             'CPF='      +ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.CPF+sLineBreak+
                             'DhCons='   +DateTimeToStr(ACBrNFe1.WebServices.ConsultaCadastro.DhCons)+sLineBreak+
                             'cUF='      +IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.cUF)+sLineBreak;

            for I:= 0 to ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Count - 1 do
             begin
              Cmd.Resposta := Cmd.Resposta+sLineBreak+
                            '[INFCAD'+Trim(IntToStrZero(I+1,3))+']'+sLineBreak+
                             'IE='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].IE+sLineBreak+
                             'CNPJ='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].CNPJ+sLineBreak+
                             'CPF='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].CPF+sLineBreak+
                             'UF='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].UF+sLineBreak+
                             'cSit='+IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].cSit)+sLineBreak+
                             'xNome='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].xNome+sLineBreak+
                             'xFant='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].xFant+sLineBreak+
                             'xRegApur='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].xRegApur+sLineBreak+
                             'CNAE='+inttostr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].CNAE)+sLineBreak+
                             'dIniAtiv='+FormatDateBr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].dIniAtiv)+sLineBreak+
                             'dUltSit='+FormatDateBr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].dUltSit)+sLineBreak+
                             'dBaixa='+FormatDateBr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].dBaixa)+sLineBreak+
                             'IEUnica='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].IEUnica+sLineBreak+
                             'IEAtual='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].IEAtual+sLineBreak+
                             'xLgr='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].xLgr+sLineBreak+
                             'nro='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].nro+sLineBreak+
                             'xCpl='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].xCpl+sLineBreak+
                             'xBairro='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].xBairro+sLineBreak+
                             'cMun='+inttostr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].cMun)+sLineBreak+
                             'xMun='+ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].xMun+sLineBreak+
                             'CEP='+inttostr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[i].CEP)+sLineBreak;
             end;

         end
        else if (Cmd.Metodo = 'criarnfe')      or (Cmd.Metodo = 'criarenviarnfe') or
                (Cmd.Metodo = 'criarnfesefaz') or (Cmd.Metodo = 'criarenviarnfesefaz') or
                (Cmd.Metodo = 'adicionarnfe')  or (Cmd.Metodo = 'adicionarnfesefaz') or
                (Cmd.Metodo = 'enviarlotenfe') or (Cmd.Metodo = 'enviardpecnfe') then
         begin
           ConfiguraDANFe(False, '');

           if (Cmd.Metodo = 'criarnfe') or (Cmd.Metodo = 'criarenviarnfe') or
              (Cmd.Metodo = 'adicionarnfe') then
           begin
               ACBrNFe1.NotasFiscais.Clear;
               ACBrNFe1.NotasFiscais.LoadFromIni( Cmd.Params(0) );
           end
           else
            begin
              if (Cmd.Metodo = 'criarnfesefaz') or (Cmd.Metodo = 'criarenviarnfesefaz') or
                 (Cmd.Metodo = 'adicionarnfesefaz') then
                  begin
                    if (copy(Cmd.Params(0), 1, 10) <> 'NOTAFISCAL') and (copy(Cmd.Params(0), 1, 11) <> 'NOTA FISCAL') then
                       if not FileExists(Cmd.Params(0)) then
                          raise Exception.Create('Arquivo '+Cmd.Params(0)+' não encontrado.');

                    ACBrNFe1.NotasFiscais.Clear;
                    ACBrNFe1.NotasFiscais.Add;
                    NFeRTXT := TNFeRTXT.Create(ACBrNFe1.NotasFiscais.Items[0].NFe);
                    try
                       if (copy(Cmd.Params(0), 1, 10) <> 'NOTAFISCAL') and (copy(Cmd.Params(0), 1, 11) <> 'NOTA FISCAL') then
                          NFeRTXT.CarregarArquivo(Cmd.Params(0))
                       else
                          NFeRTXT.ConteudoArquivo.Text := Cmd.Params(0);

                       if not NFeRTXT.LerTxt then
                          raise Exception.Create('Arquivo inválido!');
                    finally
                       NFeRTXT.Free;
                    end;
                  end;
            end;

           if (Cmd.Metodo = 'adicionarnfe')  or (Cmd.Metodo = 'adicionarnfesefaz') then
            begin
              ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(1)));
              ACBrNFe1.NotasFiscais.GerarNFe;
              Alertas := ACBrNFe1.NotasFiscais.Items[0].Alertas;
              ACBrNFe1.NotasFiscais.Assinar;
              ACBrNFe1.NotasFiscais.Validar;
              ArqNFe := PathWithDelim(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(1)))+StringReplace(ACBrNFe1.NotasFiscais.Items[0].NFe.infNFe.ID, 'NFe', '', [rfIgnoreCase])+'-nfe.xml';
              ACBrNFe1.NotasFiscais.GravarXML(ExtractFilePath(ArqNFe));
              if not FileExists(ArqNFe) then
                 raise Exception.Create('Não foi possível criar o arquivo '+ArqNFe);
            end
           else if (Cmd.Metodo = 'criarnfe')  or (Cmd.Metodo = 'criarnfesefaz') or
           (Cmd.Metodo = 'criarenviarnfe') or (Cmd.Metodo = 'criarenviarnfesefaz') then
            begin
              Salva := ACBrNFe1.Configuracoes.Geral.Salvar;
              if not Salva then
               begin
                ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
                ACBrNFe1.Configuracoes.Arquivos.PathSalvar := PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs';
               end;
              ACBrNFe1.NotasFiscais.GerarNFe;
              Alertas := ACBrNFe1.NotasFiscais.Items[0].Alertas;
              ACBrNFe1.NotasFiscais.Assinar;
              ACBrNFe1.NotasFiscais.Validar;
              ArqNFe := PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+StringReplace(ACBrNFe1.NotasFiscais.Items[0].NFe.infNFe.ID, 'NFe', '', [rfIgnoreCase])+'-nfe.xml';
              ACBrNFe1.NotasFiscais.GravarXML(ArqNFe);
              if not FileExists(ArqNFe) then
                raise Exception.Create('Não foi possível criar o arquivo '+ArqNFe);
            end;

           Cmd.Resposta := ArqNFe;
           if Alertas <> '' then
              Cmd.Resposta :=  Cmd.Resposta+sLineBreak+'Alertas:'+Alertas;
           if ((Cmd.Metodo = 'criarnfe') or (Cmd.Metodo = 'criarnfesefaz')) and (Cmd.Params(1) = '1') then
            begin
              SL := TStringList.Create;
              try
                SL.LoadFromFile(ArqNFe);
                Cmd.Resposta :=  Cmd.Resposta+sLineBreak+SL.Text;
              finally
                SL.Free;
              end;
            end;

           if (Cmd.Metodo = 'criarenviarnfe') or (Cmd.Metodo = 'criarenviarnfesefaz') or (Cmd.Metodo = 'enviarlotenfe') or (Cmd.Metodo = 'enviardpecnfe') then
            begin
              //Carregar Notas quando enviar lote
              if (Cmd.Metodo = 'enviarlotenfe')  or (Cmd.Metodo = 'enviardpecnfe') then
               begin
                 if not DirectoryExists(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(0))) then
                    raise Exception.Create('Diretório não encontrado:'+PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+trim(Cmd.Params(0)))
                 else
                  begin
                    ACBrNFe1.NotasFiscais.Clear;
                    RetFind := SysUtils.FindFirst( PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+Cmd.Params(0)+PathDelim+'*-nfe.xml', faAnyFile, SearchRec) ;
                    if (RetFind = 0) then
                     begin
                       while RetFind = 0 do
                        begin
                           CarregarDFe(PathWithDelim(ExtractFilePath(Application.ExeName))+'Lotes'+PathDelim+'Lote'+Cmd.Params(0)+PathDelim+SearchRec.Name, ArqNFe);
                           RetFind := FindNext(SearchRec);
                        end;
                        ACBrNFe1.NotasFiscais.GerarNFe;
                        ACBrNFe1.NotasFiscais.Assinar;
                        ACBrNFe1.NotasFiscais.Validar;
                     end
                    else
                       raise Exception.Create('Não foi encontrada nenhuma nota para o Lote: '+Cmd.Params(0) );
                  end;
               end;

              if (Cmd.Metodo = 'enviardpecnfe') then    //TENTAR MANTER A COMPATIBILIDADE
               begin
                { EnviadoDPEC  := ACBrNFe1.WebServices.EnviarDPEC.Executar;
                 Cmd.Resposta :=  ACBrNFe1.WebServices.EnviarDPEC.Msg+sLineBreak+
                                 '[DPEC]'+sLineBreak+
                                 'ID='+ACBrNFe1.WebServices.EnviarDPEC.ID+sLineBreak+
                                 'Versao='+ACBrNFe1.WebServices.EnviarDPEC.verAplic+sLineBreak+
                                 'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.EnviarDPEC.TpAmb)+sLineBreak+
                                 'VerAplic='+ACBrNFe1.WebServices.EnviarDPEC.VerAplic+sLineBreak+
                                 'CStat='+IntToStr(ACBrNFe1.WebServices.EnviarDPEC.cStat)+sLineBreak+
                                 'XMotivo='+ACBrNFe1.WebServices.EnviarDPEC.xMotivo+sLineBreak+
                                 'DhRegDPEC='+DateTimeToStr(ACBrNFe1.WebServices.EnviarDPEC.DhRegDPEC)+sLineBreak+
                                 'nRegDPEC='+ACBrNFe1.WebServices.EnviarDPEC.nRegDPEC+sLineBreak+
                                 'ChNFe='+ACBrNFe1.WebServices.EnviarDPEC.NFeChave+sLineBreak;
                 if (Cmd.Params(1) = '1') and ACBrNFe1.DANFE.MostrarPreview then
                  begin
                    Restaurar1.Click;
                    Application.BringToFront;
                  end;
                 ACBrNFe1.DANFE.Impressora := cbxImpressora.Text;
                 for I:= 0 to ACBrNFe1.NotasFiscais.Count-1 do
                  begin
                    if (Cmd.Params(1) = '1') and EnviadoDPEC then
                     begin
                       ACBrNFe1.DANFE.ProtocoloNFe := ACBrNFe1.WebServices.EnviarDPEC.nRegDPEC +' '+ DateTimeToStr(ACBrNFe1.WebServices.EnviarDPEC.DhRegDPEC);
                       ACBrNFe1.NotasFiscais.Items[i].Imprimir;
                     end;
                  end;
                 if (Cmd.Params(2) = '1') and ACBrNFe1.DANFE.MostrarPreview then
                    Ocultar1.Click;  }
               end
              else //enviarlotenfe
               begin
                 ACBrNFe1.WebServices.Enviar.Sincrono := IIf(Cmd.Params(3)='1',True,False);

                 if (Cmd.Metodo = 'criarenviarnfe') or (Cmd.Metodo = 'criarenviarnfesefaz') then
                  begin
                    if Trim(OnlyNumber(Cmd.Params(1))) = '' then
                       ACBrNFe1.WebServices.Enviar.Lote := '1'
                    else
                       ACBrNFe1.WebServices.Enviar.Lote := OnlyNumber(Cmd.Params(1));
                  end
                 else
                  begin
                    if Trim(OnlyNumber(Cmd.Params(0))) = '' then
                       ACBrNFe1.WebServices.Enviar.Lote := '1'
                    else
                       ACBrNFe1.WebServices.Enviar.Lote := OnlyNumber(Cmd.Params(0));
                  end;

                 ValidarIntegradorNFCe(ACBrNFe1.NotasFiscais.Items[0].NFe.infNFe.ID);
                 ACBrNFe1.WebServices.Enviar.Executar ;

                 Cmd.Resposta :=  ACBrNFe1.WebServices.Enviar.Msg+sLineBreak+
                                 '[ENVIO]'+sLineBreak+
                                 'Versao='+ACBrNFe1.WebServices.Enviar.verAplic+sLineBreak+
                                 'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Enviar.TpAmb)+sLineBreak+
                                 'VerAplic='+ACBrNFe1.WebServices.Enviar.VerAplic+sLineBreak+
                                 'CStat='+IntToStr(ACBrNFe1.WebServices.Enviar.CStat)+sLineBreak+
                                 'XMotivo='+ACBrNFe1.WebServices.Enviar.XMotivo+sLineBreak+
                                 'CUF='+IntToStr(ACBrNFe1.WebServices.Enviar.CUF)+sLineBreak+
                                 'NRec='+ACBrNFe1.WebServices.Enviar.Recibo+sLineBreak+
                                 'DhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.Enviar.dhRecbto)+sLineBreak+
                                 'TMed='+IntToStr(ACBrNFe1.WebServices.Enviar.TMed)+sLineBreak+
                                 'Msg='+ACBrNFe1.WebServices.Enviar.Msg+sLineBreak;

                 ACBrNFe1.WebServices.Retorno.Recibo := ACBrNFe1.WebServices.Enviar.Recibo;
                 ACBrNFe1.WebServices.Retorno.Executar;

                 Cmd.Resposta :=  Cmd.Resposta+
                                  ACBrNFe1.WebServices.Retorno.Msg+sLineBreak+
                                  '[RETORNO]'+sLineBreak+
                                  'Versao='+ACBrNFe1.WebServices.Retorno.verAplic+sLineBreak+
                                  'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Retorno.TpAmb)+sLineBreak+
                                  'VerAplic='+ACBrNFe1.WebServices.Retorno.VerAplic+sLineBreak+
                                  'NRec='+ACBrNFe1.WebServices.Retorno.NFeRetorno.nRec+sLineBreak+
                                  'CStat='+IntToStr(ACBrNFe1.WebServices.Retorno.CStat)+sLineBreak+
                                  'XMotivo='+ACBrNFe1.WebServices.Retorno.XMotivo+sLineBreak+
                                  'CUF='+IntToStr(ACBrNFe1.WebServices.Retorno.CUF)+sLineBreak;

                 for I:= 0 to ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Count-1 do
                  begin
                   for J:= 0 to ACBrNFe1.NotasFiscais.Count-1 do
                    begin
                     if 'NFe'+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].chNFe = ACBrNFe1.NotasFiscais.Items[j].NFe.InfNFe.Id  then
                      begin
                        Cmd.Resposta := Cmd.Resposta+
                                   '[NFE'+Trim(IntToStr(ACBrNFe1.NotasFiscais.Items[j].NFe.Ide.NNF))+']'+sLineBreak+
                                   'Versao='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].verAplic+sLineBreak+
                                   'TpAmb='+TpAmbToStr(ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].tpAmb)+sLineBreak+
                                   'VerAplic='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].verAplic+sLineBreak+
                                   'CStat='+IntToStr(ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].cStat)+sLineBreak+
                                   'XMotivo='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].xMotivo+sLineBreak+
                                   'CUF='+IntToStr(ACBrNFe1.WebServices.Retorno.NFeRetorno.cUF)+sLineBreak+
                                   'ChNFe='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].chNFe+sLineBreak+
                                   'DhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].dhRecbto)+sLineBreak+
                                   'NProt='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].nProt+sLineBreak+
                                   'DigVal='+ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[i].digVal+sLineBreak+
                                   'Arquivo='+ACBrNFe1.NotasFiscais.Items[j].NomeArq+sLineBreak;

                        //Impressão NFE enviada
                        if (cmd.Metodo = 'enviarlotenfe') then
                         begin
                          bImprimir := (Cmd.Params(1) = '1');
                          cImpressora := Cmd.Params(2);
                          bMostrarPreview := Cmd.Params(4);
                          nNumCopias := StrToIntDef(Cmd.Params(5), 0);
                          bImprimirPDF := (Cmd.Params(6) = '1');
                         end
                        else
                         begin
                          bImprimir := (Cmd.Params(2) = '1');
                          cImpressora := Cmd.Params(4);
                          bMostrarPreview := Cmd.Params(5);
                          nNumCopias := StrToIntDef(Cmd.Params(6), 0);
                          bImprimirPDF := (Cmd.Params(7) = '1');
                         end;

                        if bImprimirPDF then
                        begin
                         ACBrNFe1.NotasFiscais.Items[i].ImprimirPDF;
                         ArqPDF := OnlyNumber(ACBrNFe1.NotasFiscais.Items[i].NFe.infNFe.ID)+'-nfe.pdf';

                         Cmd.Resposta := Cmd.Resposta+
                           'PDF='+ PathWithDelim(ACBrNFe1.DANFE.PathPDF) + ArqPDF ;
                        end;

                        if ACBrNFe1.NotasFiscais.Items[i].Confirmada and bImprimir then
                         begin
                           ConfiguraDANFe(False, bMostrarPreview);

                           if nNumCopias > 0 then
                             ACBrNFe1.DANFE.NumCopias := nNumCopias;

                           if NaoEstaVazio(cImpressora) then
                             ACBrNFe1.DANFE.Impressora := cImpressora;

                           try
                             AntesDeImprimir( ( StrToBoolDef(bMostrarPreview, False) ) or
                                              (FrmACBrMonitor.cbxMostrarPreview.Checked) );
                             ACBrNFe1.NotasFiscais.Items[i].Imprimir;
                           finally
                             DepoisDeImprimir;
                           end;
                         end;

                        break;
                      end;
                    end;
                  end;
               end;
            end;
         end

        else if (Cmd.Metodo = 'enviarevento') or
                (Cmd.Metodo = 'cartadecorrecao') or
                (Cmd.Metodo = 'xmlenviarevento') then
         begin
           Cmd.Resposta := '';
           ACBrNFe1.EventoNFe.Evento.Clear;

           if (Cmd.Metodo = 'xmlenviarevento') then
            begin
              PathsNFe := TStringList.Create;
              try
                PathsNFe.Append(Cmd.Params(0));
                PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0));
                CarregarDFe(PathsNFe, ArqNFe, tDFeEventoNFe);
              finally
                PathsNFe.Free;
              end;
            end
           else
             ACBrNFe1.EventoNFe.LerFromIni( Cmd.Params(0), (Cmd.Metodo = 'cartadecorrecao') );

           ValidarIntegradorNFCe(ACBrNFe1.EventoNFe.Evento.Items[0].InfEvento.chNFe);
           ACBrNFe1.EnviarEvento(ACBrNFe1.EventoNFe.idLote);

           Cmd.Resposta := Cmd.Resposta+sLineBreak+
                           'idLote='   +IntToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.idLote)+sLineBreak+
                           'tpAmb='    +TpAmbToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.tpAmb)+sLineBreak+
                           'verAplic=' +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.verAplic+sLineBreak+
                           'cOrgao='   +IntToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.cOrgao)+sLineBreak+
                           'cStat='    +IntToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.cStat)+sLineBreak+
                           'xMotivo='  +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.xMotivo+sLineBreak;

           for I:= 0 to ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Count-1 do
            begin
              Cmd.Resposta := Cmd.Resposta+sLineBreak+
               '[EVENTO'+Trim(IntToStrZero(I+1,3))+']'+sLineBreak+
               'id='        +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.Id+sLineBreak+
               'tpAmb='     +TpAmbToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.tpAmb)+sLineBreak+
               'verAplic='  +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.verAplic+sLineBreak+
               'cOrgao='    +IntToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.cOrgao)+sLineBreak+
               'cStat='     +IntToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.cStat)+sLineBreak+
               'xMotivo='   +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.xMotivo+sLineBreak+
               'chNFe='     +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.chNFe+sLineBreak+
               'tpEvento='  +TpEventoToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.tpEvento)+sLineBreak+
               'xEvento='   +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.xEvento+sLineBreak+
               'nSeqEvento='+IntToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.nSeqEvento)+sLineBreak+
               'CNPJDest='  +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.CNPJDest+sLineBreak+
               'emailDest=' +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.emailDest+sLineBreak+
               'dhRegEvento='+DateTimeToStr(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.dhRegEvento)+sLineBreak+
               'nProt='     +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.nProt+sLineBreak+
               'Arquivo='   +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.NomeArquivo+sLineBreak+
               'XML='       +ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[I].RetInfEvento.XML+sLineBreak;
            end;
         end

        //NFe.ConsultaNFeDest(cCNPJ,nIndicadorNFe,nIndicadorEmissor,nUltNSU)
        else if Cmd.Metodo = 'consultanfedest' then
         begin
           if not ValidarCNPJ(Cmd.Params(0)) then
              raise Exception.Create('CNPJ '+Cmd.Params(0)+' inválido.');

           ValidarIntegradorNFCe();
           try
              ACBrNFe1.ConsultaNFeDest(Cmd.Params(0),
                                       StrToIndicadorNFe(ok,Cmd.Params(1)),
                                       StrToIndicadorEmissor(ok,Cmd.Params(2)),
                                       Cmd.Params(3));

              Cmd.Resposta := Cmd.Resposta+sLineBreak+
                              '[CONSULTANFEDEST]'+sLineBreak+
                              'versao='   +ACBrNFe1.WebServices.ConsNFeDest.retConsNFeDest.versao+sLineBreak+
                              'tpAmb='    +TpAmbToStr(ACBrNFe1.WebServices.ConsNFeDest.retConsNFeDest.tpAmb)+sLineBreak+
                              'verAplic=' +ACBrNFe1.WebServices.ConsNFeDest.retConsNFeDest.verAplic+sLineBreak+
                              'cStat='    +IntToStr(ACBrNFe1.WebServices.ConsNFeDest.retConsNFeDest.cStat)+sLineBreak+
                              'xMotivo='  +ACBrNFe1.WebServices.ConsNFeDest.retConsNFeDest.xMotivo+sLineBreak+
                              'dhResp='   +DateTimeToStr(ACBrNFe1.WebServices.ConsNFeDest.retConsNFeDest.dhResp)+sLineBreak+
                              'indCont='  +IndicadorContinuacaoToStr(ACBrNFe1.WebServices.ConsNFeDest.retConsNFeDest.indCont)+sLineBreak+
                              'ultNSU='   +ACBrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ultNSU+sLineBreak;

              J := 1;
              for I:= 0 to AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Count-1 do
               begin
                 if Trim(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.chNFe) <> '' then
                  begin
                    Cmd.Resposta := Cmd.Resposta+sLineBreak+
                     '[RESNFE'+Trim(IntToStrZero(J,3))+']'+sLineBreak+
                     'NSU='     +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.NSU+sLineBreak+
                     'chNFe='   +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.chNFe+sLineBreak+
                     'CNPJ='    +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.CNPJCPF+sLineBreak+
                     'xNome='   +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.xNome+sLineBreak+
                     'IE='      +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.IE+sLineBreak+
                     'dEmi='    +DateTimeToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.dEmi)+sLineBreak+
                     'tpNF='    +tpNFToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.tpNF)+sLineBreak+
                     'vNF='     +FloatToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.vNF)+sLineBreak+
                     'digVal='  +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.digVal+sLineBreak+
                     'dhRecbto='+DateTimeToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.dhRecbto)+sLineBreak+
                     'cSitNFe=' +SituacaoDFeToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.cSitNFe)+sLineBreak+
                     'cSitConf='+SituacaoManifDestToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resNFe.cSitConf)+sLineBreak;
                     J := J + 1;
                  end;
               end;

              J := 1;
              for i := 0 to AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Count -1 do
               begin
                 if Trim(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.chNFe) <> '' then
                  begin
                    Cmd.Resposta := Cmd.Resposta+sLineBreak+
                     '[RESCANC'+Trim(IntToStrZero(J,3))+']'+sLineBreak+
                     'NSU='     +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.NSU+sLineBreak+
                     'chNFe='   +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.chNFe+sLineBreak+
                     'CNPJ='    +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.CNPJCPF+sLineBreak+
                     'xNome='   +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.xNome+sLineBreak+
                     'IE='      +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.IE+sLineBreak+
                     'dEmi='    +DateTimeToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.dEmi)+sLineBreak+
                     'tpNF='    +tpNFToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.tpNF)+sLineBreak+
                     'vNF='     +FloatToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.vNF)+sLineBreak+
                     'digVal='  +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.digVal+sLineBreak+
                     'dhRecbto='+DateTimeToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.dhRecbto)+sLineBreak+
                     'cSitNFe=' +SituacaoDFeToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.cSitNFe)+sLineBreak+
                     'cSitConf='+SituacaoManifDestToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCanc.cSitConf)+sLineBreak;
                     J := J + 1;
                  end;
               end;

              J := 1;
              for i := 0 to AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Count -1 do
               begin
                  if Trim(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.chNFe) <> '' then
                   begin
                    Cmd.Resposta := Cmd.Resposta+sLineBreak+
                     '[RESCCE'+Trim(IntToStrZero(J,3))+']'+sLineBreak+
                     'NSU='       +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.NSU+sLineBreak+
                     'chNFe='     +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.chNFe+sLineBreak+
                     'dhEvento='  +DateTimeToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.dhEvento)+sLineBreak+
                     'tpEvento='  +TpEventoToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.tpEvento)+sLineBreak+
                     'nSeqEvento='+IntToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.nSeqEvento)+sLineBreak+
                     'descEvento='+AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.descEvento+sLineBreak+
                     'xCorrecao=' +AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.xCorrecao+sLineBreak+
                     'tpNF='      +tpNFToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.tpNF)+sLineBreak+
                     'dhRecbto='  +DateTimeToStr(AcbrNFe1.WebServices.ConsNFeDest.retConsNFeDest.ret.Items[i].resCCe.dhRecbto)+sLineBreak;
                     J := J + 1;
                   end;
               end;
           except
             on E: Exception do
              begin
                raise Exception.Create(AcbrNFe1.WebServices.ConsNFeDest.Msg+sLineBreak+E.Message);
              end;
           end;
         end

        else if (Cmd.Metodo = 'distribuicaodfe') or            //NFe.DistribuicaoDFe(cUF,cCNPJ,nUltNSU,nNSU,aChNFe)
                (Cmd.Metodo = 'distribuicaodfeporchavenfe') or //NFe.DistribuicaoDFePorChaveNFe(cUF, cCNPJ, aChNFe)
                (Cmd.Metodo = 'distribuicaodfepornsu') or      //NFe.DistribuicaoDFePorNSU(cUF, cCNPJ, nNSU)
                (Cmd.Metodo = 'distribuicaodfeporultnsu')then  //NFe.DistribuicaoDFePorUltNSU(cUF, cCNPJ, nUltNSU)
         begin
           if not ValidarCNPJ(Cmd.Params(1)) then
              raise Exception.Create('CNPJ '+Cmd.Params(1)+' inválido.');

           ValidarIntegradorNFCe();
           try
              if Cmd.Metodo = 'distribuicaodfeporchavenfe' then
                ACBrNFe1.DistribuicaoDFePorChaveNFe(StrToIntDef(Cmd.Params(0),0),Cmd.Params(1),Cmd.Params(2))
              else if Cmd.Metodo = 'distribuicaodfepornsu' then
                ACBrNFe1.DistribuicaoDFePorNSU(StrToIntDef(Cmd.Params(0),0),Cmd.Params(1),Cmd.Params(2))
              else if Cmd.Metodo = 'distribuicaodfeporultnsu' then
                ACBrNFe1.DistribuicaoDFePorUltNSU(StrToIntDef(Cmd.Params(0),0),Cmd.Params(1),Cmd.Params(2))
              else
                ACBrNFe1.DistribuicaoDFe(StrToIntDef(Cmd.Params(0),0),Cmd.Params(1),Cmd.Params(2),Cmd.Params(3),Cmd.Params(4));

              if ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.cStat = 137 then
                sTemMais := '1'
             else
                sTemMais := '0'; //pog para facilitar a indicacao de continuidade

             Cmd.Resposta:= Cmd.Resposta+sLineBreak+
                            '[DISTRIBUICAODFE]'+sLineBreak+
                            'versao='  +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.versao+sLineBreak+
                            'tpAmb='   +TpAmbToStr(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.tpAmb)+sLineBreak+
                            'verAplic='+ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.verAplic+sLineBreak+
                            'cStat='   +IntToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.cStat)+sLineBreak+
                            'xMotivo=' +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo+sLineBreak+
                            'dhResp='  +DateTimeToStr(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.dhResp)+sLineBreak+
                            'indCont=' +sTemMais+sLineBreak+
                            'ultNSU='  +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.ultNSU+sLineBreak+
                            'maxNSU='  +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.maxNSU+sLineBreak+
                            'NomeArq=' +ACBrNFe1.WebServices.DistribuicaoDFe.NomeArq;
             J := 1;
             for i:= 0 to AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count-1 do
             begin
              if Trim(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.chNFe) <> '' then
                 begin
                      Cmd.Resposta := Cmd.Resposta+sLineBreak+
                       '[RESNFE'+Trim(IntToStrZero(J,3))+']'+sLineBreak+
                       'NSU='     +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].NSU+sLineBreak+
                       'chNFe='   +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.chNFe+sLineBreak+
                       'CNPJ='    +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.CNPJCPF+sLineBreak+
                       'xNome='   +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.xNome+sLineBreak+
                       'IE='      +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.IE+sLineBreak+
                       'dEmi='    +DateTimeToStr(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.dhEmi)+sLineBreak+
                       'tpNF='    +tpNFToStr(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.tpNF)+sLineBreak+
                       'vNF='     +FloatToStr(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.vNF)+sLineBreak+
                       'digVal='  +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.digVal+sLineBreak+
                       'dhRecbto='+DateTimeToStr(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.dhRecbto)+sLineBreak+
                       'cSitNFe=' +SituacaoDFeToStr(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.cSitNFe)+sLineBreak+
                       'nProt='   +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resNFe.nProt+sLineBreak+
                       'XML='     +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].XML;

                      Cmd.Resposta := Cmd.Resposta + sLineBreak +
                       'NomeArq=' + ACBrNFe1.WebServices.DistribuicaoDFe.listaArqs[i] + sLineBreak +
                       'schema='  + GetEnumName(typeInfo(TSchemaNFe), Ord(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].schema) );

                       inc(J);
                    end;
                 end;
             J := 1;
             for i:= 0 to AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count-1 do
              begin
                if Trim(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resEvento.chNFe) <> '' then
                   begin
                    Cmd.Resposta := Cmd.Resposta+sLineBreak+
                     '[RESEVE'+Trim(IntToStrZero(J,3))+']'+sLineBreak+
                     'NSU='       +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].NSU+sLineBreak+
                     'chNFe='     +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resEvento.chNFe+sLineBreak+
                     'dhEvento='  +DateTimeToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resEvento.dhEvento)+sLineBreak+
                     'tpEvento='  +TpEventoToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resEvento.tpEvento)+sLineBreak+
                     'nSeqEvento='+IntToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resEvento.nSeqEvento)+sLineBreak+
                     'cOrgao='    +IntToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resEvento.cOrgao)+sLineBreak+
                     'CNPJ='      +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resEvento.CNPJCPF+sLineBreak+
                     'dhRecbto='  +DateTimeToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resEvento.dhRecbto)+sLineBreak+
                     'nProt='     +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].resEvento.nProt+sLineBreak+
                     'XML='       +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].XML;

                    Cmd.Resposta := Cmd.Resposta + sLineBreak +
                     'NomeArq='   + ACBrNFe1.WebServices.DistribuicaoDFe.listaArqs[i] + sLineBreak +
                     'schema='    + GetEnumName(typeInfo(TSchemaNFe), Ord(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].schema) );

                     inc(J);
                   end;
               end;
              J := 1;
              for i:= 0 to AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count-1 do
               begin
                 if Trim(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.versao) <> '' then
                    begin
                     Cmd.Resposta := Cmd.Resposta+sLineBreak+
                      '[ProEve'     +Trim(IntToStrZero(J,3))+']'+sLineBreak+
                      'NSU='        +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].NSU+sLineBreak+
                      'chNFe='      +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.chNFe+sLineBreak+
                      'cOrgao='     +IntToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.cOrgao)+sLineBreak+
                      'CNPJ='       +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.CNPJ+sLineBreak+
                      'id='         +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.Id+sLineBreak+
                      'dhEvento='   +DateTimeToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.dhEvento)+sLineBreak+
                      'nSeqEvento=' +IntToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.nSeqEvento)+sLineBreak+
                      'tpAmb='      +TpAmbToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.tpAmb)+sLineBreak+
                      'tpEvento='   +TpEventoToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.tpEvento)+sLineBreak+
                      'verEvento='  +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.verEvento+sLineBreak+
                      'desEvento='  +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.descEvento+sLineBreak+
                      //descricao
                      'xJust='      +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.xJust+sLineBreak+
                      'xMotivo='    +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.xCorrecao+sLineBreak+
                      //emit
                      'EmiCnpj='    +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.emit.CNPJ+sLineBreak+
                      'EmiIe='      +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.emit.IE+sLineBreak+
                      'EmixNome='   +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.emit.xNome+sLineBreak+
                      //cte
                      'cteNProt='   +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.nProt+sLineBreak+
                      'cteChvCte='  +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.chCTe+sLineBreak+
                      'cteDhemi='   +DateTimeToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.dhEmi)+sLineBreak+
                      'cteModal='   +TpModalToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.modal)+sLineBreak+
                      'cteDhRebcto='+DateTimeToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.detEvento.CTe.dhRecbto)+sLineBreak+
                      'XML='        +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].XML;

                     Cmd.Resposta := Cmd.Resposta + sLineBreak +
                      'NomeArq='    + ACBrNFe1.WebServices.DistribuicaoDFe.listaArqs[i] + sLineBreak +
                      'schema='     + GetEnumName(typeInfo(TSchemaNFe), Ord(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].schema) );

                     inc(J);
                    end;
                end;
              J := 1;
              for i:= 0 to AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count-1 do
               begin
                 if Trim(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.Id) <> '' then
                    begin
                     Cmd.Resposta := Cmd.Resposta+sLineBreak+
                     '[InfEve'     +Trim(IntToStrZero(J,3))+']'+sLineBreak+
                     'id='         +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.Id+sLineBreak+
                     'verAplic='   +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.verAplic+sLineBreak+
                     'tpAmb='      +TpAmbToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.tpAmb)+sLineBreak+
                     'cOrgao='     +IntToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.cOrgao)+sLineBreak+
                     'chNfe='      +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.chNFe+sLineBreak+
                     'cStat='      +IntToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.cStat)+sLineBreak+
                     'CnpjDest='   +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.CNPJDest+sLineBreak+
                     'cOrgaoAutor='+IntToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.cOrgaoAutor)+sLineBreak+
                     'tpEvento='   +TpEventoToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.tpEvento)+sLineBreak+
                     'nSeqEvento=' +IntToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.nSeqEvento)+sLineBreak+
                     'xEvento='    +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.xEvento+sLineBreak+
                     'xMotivo='    +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.xMotivo+sLineBreak+
                     'dhRegEvento='+DateTimeToStr(AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.dhRegEvento)+sLineBreak+
                     'emailDest='  +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.emailDest+sLineBreak+
                     'nProt='      +AcbrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].procEvento.RetinfEvento.nProt+sLineBreak+
                     'XML='        +ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].XML;

                     Cmd.Resposta := Cmd.Resposta + sLineBreak +
                     'NomeArq='    + ACBrNFe1.WebServices.DistribuicaoDFe.listaArqs[i] + sLineBreak +
                     'schema='     + GetEnumName(typeInfo(TSchemaNFe), Ord(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[i].schema) );

                     inc(J);
                    end;
                end;
           except
             on E: Exception do
              begin
                raise Exception.Create(AcbrNFe1.WebServices.DistribuicaoDFe.Msg+sLineBreak+E.Message);
              end;
           end;
         end

        //NFe.DownloadNFe(cCNPJ,cChaves)
        else if Cmd.Metodo = 'downloadnfe' then
         begin
           if not ValidarCNPJ(Cmd.Params(0)) then
              raise Exception.Create('CNPJ '+Cmd.Params(0)+' inválido.');

           with ACBrNFe1.DownloadNFe do
           begin
             Download.CNPJ := Cmd.Params(0);

             if NaoEstaVazio(Cmd.Params(1)) then
             begin
               ChavesNFe:=TstringList.Create;
               try
                  ChavesNFe.DelimitedText := sLineBreak;
                  ChavesNFe.Text := StringReplace(Cmd.Params(1),';',sLineBreak,[rfReplaceAll]);
                  Download.Chaves.Clear;
                  for I := 0 to ChavesNFe.Count - 1 do
                  begin
                     if not ValidarChave(ChavesNFe.Strings[i]) then
                        raise Exception.Create('Chave '+ChavesNFe.Strings[i]+' inválida.');

                     with Download.Chaves.Add do
                     begin
                       chNFe := ChavesNFe.Strings[i];
                     end;
                  end;
               finally
                  ChavesNFe.Free;
               end;
             end;
           end;

           try
              ACBrNFe1.Download;

              Cmd.Resposta := ACBrNFe1.WebServices.DownloadNFe.Msg+sLineBreak+
                              '[DOWNLOADNFE]'+sLineBreak+
                              'versao='   +ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.versao+sLineBreak+
                              'tpAmb='    +TpAmbToStr(ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.tpAmb)+sLineBreak+
                              'verAplic=' +ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.verAplic+sLineBreak+
                              'cStat='    +IntToStr(ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.cStat)+sLineBreak+
                              'xMotivo='  +ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.xMotivo+sLineBreak+
                              'dhResp='   +DateTimeToStr(ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.dhResp)+sLineBreak;

              for i := 0 to ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.retNFe.Count -1 do
               begin
                 ArqNFe := PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+
                           ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.retNFe.Items[i].chNFe+'-down-nfe.xml' ;

                 try
                   WriteToTXT( ArqNFe, ConverteXMLtoUTF8(ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.retNFe.Items[i].procNFe), False, False);
                 except
                   on E: Exception do
                   begin
                     Cmd.Resposta := Cmd.Resposta+sLineBreak+'Erro ao escrever no arquivo: '+ArqNFe+' ['+E.Message+']';
                   end;
                 end;

                 Cmd.Resposta := Cmd.Resposta+sLineBreak+
                                 '[NFE'+Trim(IntToStrZero(I+1,3))+']'+sLineBreak+
                                 'ChNFe='+ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.retNFe.Items[i].chNFe+sLineBreak+
                                 'cStat='+IntToStr(ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.retNFe.Items[i].cStat)+sLineBreak+
                                 'xMotivo='+ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.retNFe.Items[i].xMotivo+sLineBreak+
                                 'Arquivo='+ArqNFe+sLineBreak+
                                 'procNFe='+ACBrNFe1.WebServices.DownloadNFe.retDownloadNFe.retNFe.Items[i].procNFe+sLineBreak;
               end;
           except
             on E: Exception do
              begin
                raise Exception.Create(ACBrNFe1.WebServices.DownloadNFe.Msg+sLineBreak+E.Message);
              end;
           end;
         end

        //NFe.EnviarEmail(cEmailDestino,cArqXML,cEnviaPDF,[cAssunto],[cEmailsCopias],[cAnexos])
        else if Cmd.Metodo = 'enviaremail' then
         begin
           ACBrNFe1.NotasFiscais.Clear;
           PathsNFe := TStringList.Create;
           try
             PathsNFe.Append(Cmd.Params(1));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
             CarregarDFe(PathsNFe, ArqNFe);
           finally
             PathsNFe.Free;
           end;

           ConfiguraDANFe(True, '');

           sMensagemEmail := TStringList.Create;
           CC := TstringList.Create;
           Anexos := TstringList.Create;
           try
             sMensagemEmail.Text := SubstituirVariaveis( mmEmailMsgNFe.Lines.Text );

             CC.DelimitedText := sLineBreak;
             CC.Text := StringReplace(Cmd.Params(4),';',sLineBreak,[rfReplaceAll]);

             Anexos.DelimitedText := sLineBreak;
             Anexos.Text := StringReplace(Cmd.Params(5),';',sLineBreak,[rfReplaceAll]);

             ACBrNFe1.NotasFiscais.Items[0].EnviarEmail( Cmd.Params(0),
                                                         SubstituirVariaveis( IfThen(NaoEstaVazio(Cmd.Params(3)),Cmd.Params(3),edtEmailAssuntoNFe.Text) ),
                                                         sMensagemEmail
                                                         , (Cmd.Params(2) = '1')   // Enviar PDF junto
                                                         , CC    // Lista com emails que serão enviado cópias - TStrings
                                                         , Anexos); // Lista de anexos - TStrings


             Cmd.Resposta := 'Email enviado com sucesso';
           finally
             CC.Free;
             Anexos.Free;
             sMensagemEmail.Free;
           end;
        end

        //NFe.EnviarEmailEvento(cEmailDestino,cArqXMLEvento,[cArqXMLNFe],cEnviaPDF,[cAssunto],[cEmailsCopias],[cAnexos])
        else if Cmd.Metodo = 'enviaremailevento' then
         begin
           ACBrNFe1.EventoNFe.Evento.Clear;
           PathsNFe := TStringList.Create;
           try
             PathsNFe.Append(Cmd.Params(1));
             PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
             CarregarDFe(PathsNFe, ArqEvento, tDFeEventoNFe);
           finally
             PathsNFe.Clear;
           end;

           ACBrNFe1.NotasFiscais.Clear;
           if NaoEstaVazio(Cmd.Params(2)) then
           begin
             try
               PathsNFe.Append(Cmd.Params(2));
               PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(2));
               CarregarDFe(PathsNFe, ArqNFe);
             finally
               PathsNFe.Clear;
             end;
           end;

           ConfiguraDANFe(True, '');

           if (Cmd.Params(3) = '1') then
            begin
              try
                 ACBrNFe1.ImprimirEventoPDF;

                 ArqPDF := OnlyNumber(ACBrNFe1.EventoNFe.Evento[0].InfEvento.id);
                 ArqPDF := PathWithDelim(ACBrNFe1.DANFE.PathPDF)+ArqPDF+'-procEventoNFe.pdf';
              except
                 raise Exception.Create('Erro ao criar o arquivo PDF');
              end;
            end;

           sMensagemEmail := TStringList.Create;
           CC:=TstringList.Create;
           Anexos:=TstringList.Create;
           try
             sMensagemEmail.Text := SubstituirVariaveis( mmEmailMsgNFe.Lines.Text );

             CC.DelimitedText := sLineBreak;
             CC.Text := StringReplace(Cmd.Params(5),';',sLineBreak,[rfReplaceAll]);

             Anexos.DelimitedText := sLineBreak;
             Anexos.Text := StringReplace(Cmd.Params(6),';',sLineBreak,[rfReplaceAll]);

             // Se carregou evento usando XML como parâmetro, salva XML para poder anexar
             if ( ArqEvento = '' ) then
             begin
               tipoEvento := ACBrNFe1.EventoNFe.Evento[0].InfEvento.tpEvento;
               ArqEvento  := ACBrNFe1.EventoNFe.ObterNomeArquivo(tipoEvento);
               ArqEvento  := PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.GetPathEvento(tipoEvento))+ArqEvento;
               ACBrNFe1.EventoNFe.Gerador.SalvarArquivo(ArqEvento);
             end;
             Anexos.Add(ArqEvento);

             if (Cmd.Params(3) = '1') then
                Anexos.Add(ArqPDF);

             ACBrNFe1.EnviarEmail(
               Cmd.Params(0),
               SubstituirVariaveis( IfThen(NaoEstaVazio(Cmd.Params(4)),Cmd.Params(4),edtEmailAssuntoNFe.Text) ),
               sMensagemEmail,
               CC,
               Anexos
             );

             Cmd.Resposta := 'Email enviado com sucesso';

           finally
             CC.Free;
             Anexos.Free;
             sMensagemEmail.Free;
           end;
         end

        else if Cmd.Metodo = 'enviaremailinutilizacao' then //NFe.EnviarEmailInutilizacao(cEmailDestino,cArqXMLInutilizacao,cEnviaPDF,[cAssunto],[cEmailsCopias],[cAnexos])
         begin
           PathsNFe := TStringList.Create;
           try
              PathsNFe.Append(Cmd.Params(1));
              PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1));
              PathsNFe.Append(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(1)+'-inu.xml');
              CarregarDFe(PathsNFe, ArqEvento, tDFeInutNFe);
           finally
             PathsNFe.Free;
           end;

           ConfiguraDANFe(True, '');

           if (Cmd.Params(2) = '1') then
            begin
              try
                 ACBrNFe1.ImprimirInutilizacaoPDF;
                 ArqPDF := OnlyNumber(ACBrNFe1.InutNFe.ID);
                 ArqPDF := PathWithDelim(ACBrNFe1.DANFE.PathPDF)+ArqPDF+'-procInutNFe.pdf';
              except
                 raise Exception.Create('Erro ao criar o arquivo PDF');
              end;
            end;

           sMensagemEmail := TStringList.Create;
           CC:=TstringList.Create;
           Anexos:=TstringList.Create;
           try
             sMensagemEmail.Text := SubstituirVariaveis( mmEmailMsgNFe.Lines.Text );

             CC.DelimitedText := sLineBreak;
             CC.Text := StringReplace(Cmd.Params(4),';',sLineBreak,[rfReplaceAll]);

             Anexos.DelimitedText := sLineBreak;
             Anexos.Text := StringReplace(Cmd.Params(5),';',sLineBreak,[rfReplaceAll]);

             // Se carregou evento usando XML como parâmetro, salva XML para poder anexar
             if ( ArqEvento = '' ) then
             begin
               tipoEvento := ACBrNFe1.EventoNFe.Evento[0].InfEvento.tpEvento;
               ArqEvento  := ACBrNFe1.EventoNFe.ObterNomeArquivo(tipoEvento);
               ArqEvento  := PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.GetPathEvento(tipoEvento))+ArqEvento;
               ACBrNFe1.EventoNFe.Gerador.SalvarArquivo(ArqEvento);
             end;
             Anexos.Add(ArqEvento);

             if (Cmd.Params(2) = '1') then
                Anexos.Add(ArqPDF);


             ACBrNFe1.EnviarEmail(
                Cmd.Params(0),
                SubstituirVariaveis( IfThen(NaoEstaVazio(Cmd.Params(3)),Cmd.Params(3),edtEmailAssuntoNFe.Text) ),
                sMensagemEmail,
                CC,
                Anexos
             );

             Cmd.Resposta := 'Email enviado com sucesso';

           finally
             CC.Free;
             Anexos.Free;
             sMensagemEmail.Free;
           end;
         end

        else if Cmd.Metodo = 'setcertificado' then //NFe.SetCertificado(cCertificado,cSenha)
         begin
           DoACBr(Cmd);
         end

        else if Cmd.Metodo = 'setambiente' then //NFe.SetAmbiente(nNumAmbiente) 1-Produção 2-Homologação
         begin
           if (StrToInt(Cmd.Params(0))>=1) and (StrToInt(Cmd.Params(0))<=2) then
            begin
              ACBrNFe1.Configuracoes.WebServices.Ambiente := StrToTpAmb(OK, Cmd.Params(0));
              rgTipoAmb.ItemIndex := ACBrNFe1.Configuracoes.WebServices.AmbienteCodigo-1;
              SalvarIni;
            end
           else
              raise Exception.Create('Ambiente Inválido.');
         end

        else if Cmd.Metodo = 'setlogomarca' then
        begin
          if FileExists(Cmd.Params(0)) then
           begin
             ACBrNFe1.DANFE.Logo       := Cmd.Params(0);
             if (Cmd.Params(1) = '1') then
               edtLogoMarcaNFCeSAT.Text  := ACBrNFe1.DANFE.Logo
             else
               edtLogoMarca.Text         := ACBrNFe1.DANFE.Logo;
             SalvarIni;
           end
          else
             raise Exception.Create('Arquivo não encontrado.');
        end

        else if Cmd.Metodo = 'setformaemissao' then //NFe.SetFormaEmissao(nFormaEmissao)
         begin
           if cbModoEmissao.checked then
             exit;

           OK := False;
           FormaEmissao := StrToTpEmis(OK, Cmd.Params(0));

           if not OK then
             raise Exception.Create('Forma de Emissão Inválida: '+TpEmisToStr(FormaEmissao))
           else
           begin
             if FormaEmissao in [teSVCSP] then
                raise Exception.Create('Forma de Emissão Inválida para NFe/NFCe '+TpEmisToStr(FormaEmissao))
             else
             begin
               ACBrNFe1.Configuracoes.Geral.FormaEmissao := StrToTpEmis(OK, Cmd.Params(0));
               cbFormaEmissaoNFe.ItemIndex := ACBrNFe1.Configuracoes.Geral.FormaEmissaoCodigo-1;
               SalvarIni;
             end
           end;
         end

        else if Cmd.Metodo = 'setversaodf' then //NFe.SetVersaoDF(nVersao) 2.00 3.00 3.10
         begin
            VersaoDF := StrToVersaoDF(OK, Cmd.Params(0));
            if OK then
             begin
               ACBrNFe1.Configuracoes.Geral.VersaoDF := VersaoDF;
               cbVersaoWS.ItemIndex := cbVersaoWS.Items.IndexOf(Cmd.Params(0)) ;
               SalvarIni;
             end
            else
              raise Exception.Create('Versão Inválida.');
         end

        else if Cmd.Metodo = 'setmodelodf' then //NFe.SetModeloDF(nModeloDF) 55 65
         begin
            ModeloDF := StrToModeloDF(OK, Cmd.Params(0));
            if OK then
            begin
               ACBrNFe1.Configuracoes.Geral.ModeloDF := ModeloDF;
               ValidarIntegradorNFCe();
            end
            else
              raise Exception.Create('Modelo Inválido(55/65).');
         end

        else if (Cmd.Metodo = 'settoken') or (Cmd.Metodo = 'setcsc') then //NFe.SetCSC(cCSC)
         begin
           ACBrNFe1.Configuracoes.Geral.CSC := Cmd.Params(0);
           edtToken.Text := ACBrNFe1.Configuracoes.Geral.CSC;

           if (Cmd.Params(1) <> '') then
            begin
              ACBrNFe1.Configuracoes.Geral.IdCSC := Cmd.Params(1);
              edtIdToken.Text := ACBrNFe1.Configuracoes.Geral.IdCSC;
            end;

           SalvarIni;
         end

        else if (Cmd.Metodo = 'setidtoken') or (Cmd.Metodo = 'setidcsc') then  //NFe.SetIdCSC(cIdCSC)
        begin
          ACBrNFe1.Configuracoes.Geral.IdCSC := Cmd.Params(0);
          edtIdToken.Text := ACBrNFe1.Configuracoes.Geral.IdCSC;
                    if (Cmd.Params(1) <> '') then
           begin
             ACBrNFe1.Configuracoes.Geral.CSC := Cmd.Params(1);
             edtToken.Text := ACBrNFe1.Configuracoes.Geral.CSC;
           end;
           SalvarIni;
        end

        else if Cmd.Metodo = 'settipoimpressao' then
        begin
          TipoDANFE := StrToTpImp(OK, Cmd.Params(0));
          if OK then
          begin
            ACBrNFe1.DANFE.TipoDANFE := TipoDANFE;
            rgTipoDanfe.ItemIndex := StrToIntDef(TpImpToStr(TipoDANFE),1) -1 ;
            SalvarIni;
          end
          else
            raise Exception.Create('Tipo Impressão Inválido.');
        end

        else if (Cmd.Metodo = 'lernfe') or (Cmd.Metodo = 'gerarininfe')  then
         begin
           try
             ACBrNFe1.NotasFiscais.Clear;
             CarregarDFe(Cmd.Params(0), ArqNFe);
             Cmd.Resposta := ACBrNFe1.NotasFiscais.GerarIni();
           except
           on E: Exception do
             begin
               raise Exception.Create('Erro ao gerar INI da NFe.'+sLineBreak+E.Message);
             end;
           end;
         end

        else if Cmd.Metodo = 'nfetotxt' then  //NFe.NFetoTXT(cArqXML,cNomeArqTXT)
         begin
           ACBrNFe1.NotasFiscais.Clear;
           CarregarDFe(Cmd.Params(0), ArqNFe);

           ACBrNFe1.NotasFiscais.Items[0].GravarTXT(ChangeFileExt(ACBrNFe1.NotasFiscais.Items[0].NomeArq,'.txt'),Cmd.Params(1));
           Cmd.Resposta := ChangeFileExt(ACBrNFe1.NotasFiscais.Items[0].NomeArq,'.txt');
         end

        else if Cmd.Metodo = 'savetofile' then //NFe.SavetoFile(cNomeArq,cConteudoArq)
         begin
           Lines := TStringList.Create ;
           try
              Lines.Clear ;
              Lines.Text := ConvertStrRecived( cmd.Params(1) );
              Lines.SaveToFile( Cmd.Params(0) );
           finally
              Lines.Free ;
           end ;
         end

        else if Cmd.Metodo = 'loadfromfile' then //NFe.LoadfromFile(cNomeArq,nSegundos)
         begin
           Files := Cmd.Params(0) ;
           dtFim := IncSecond(now, StrToIntDef(Cmd.Params(1),1) ) ;
           while now <= dtFim do
           begin
              if FileExists( Files ) then
              begin
                 Lines  := TStringList.Create ;
                 try
                    Lines.Clear ;
                    Lines.LoadFromFile( Files ) ;
                    Cmd.Resposta := Lines.Text ;
                    Break ;
                 finally
                    Lines.Free ;
                 end ;
              end ;

              {$IFNDEF NOGUI}
               Application.ProcessMessages ;
              {$ENDIF}
              sleep(100) ;
           end ;

           if not FileExists( Cmd.Params(0) ) then
              raise Exception.Create('Arquivo '+Cmd.Params(0)+' não encontrado')
         end

        else if Cmd.Metodo = 'fileexists' then  //NFe.FileExists(cNomeArq)
         begin
           if not FileExists( Cmd.Params(0) ) then
              raise Exception.Create('Arquivo '+Cmd.Params(0)+' não encontrado')
         end


        else if ((Cmd.Metodo = 'certificadodatavencimento') or (Cmd.Metodo = 'datavencimentocertificado')) then //NFe.CertificadoDataVencimento
         begin
           Cmd.Resposta := DateToStr(ACBrNFe1.SSL.CertDataVenc);
         end

        else if Cmd.Metodo = 'cnpjcertificado' then //NFe.CNPJCertificado
         begin
           Cmd.Resposta := ACBrNFe1.SSL.CertCNPJ ;
         end

        else if Cmd.Metodo = 'lerini' then //NFe.LerIni  Recarrega configurações do arquivo INI
           LerIni

        else if Cmd.Metodo = 'gerarchave' then //NFe.GerarChave(codigoUF,codigoNumerico,modelo,serie,numero,tpemi,emissao,CNPJ)
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

        else if cmd.Metodo = 'getpathnfe' then //NFe.GetPathNFe
           Cmd.Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathNFe

        else if Cmd.Metodo = 'getpathcce' then //NFe.GetPathCCE Retorna Path onde está sendo salvo o xml das CCEs
           Cmd.Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathEvento(teCCe)

        else if Cmd.Metodo = 'getpathcan' then //NFe.GetPathCan Path Eventos de cancelamento
           Cmd.Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathEvento(teCancelamento)

        else if Cmd.Metodo = 'getpathevento' then //NFe.GetPathEvento
           Cmd.Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathEvento( TpcnTpEvento(StrToInt(Cmd.Params(0))) )

        else if cmd.Metodo = 'getpathinu' then //NFe.GetPathInu
           Cmd.Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathInu(Cmd.Params(0))

        else if Cmd.Metodo = 'imprimirrelatorio' then //NFe.ImprimirRelatorio(cTexto)
         begin
           ConfiguraDANFe(False, '');

           if rgModeloDANFeNFCE.ItemIndex <> 1  then
               raise Exception.Create('Comando disponível apenas para o DANFe modelo DANFe ESCPOS');

           MemoTXT := TMemo.Create(FrmACBrMonitor) ;
           try
              MemoTXT.Clear ;
              MemoTXT.Text := ConvertStrRecived( cmd.Params(0) );
              if not ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativo then
                 ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativar;
              ACBrNFeDANFeESCPOS1.ImprimirRelatorio(MemoTXT.Lines);
           finally
              MemoTXT.Free ;
           end ;

         end

        else if Cmd.Metodo = 'restaurar' then //NFe.Restaurar
           Restaurar1Click( FrmACBrMonitor )

        else if Cmd.Metodo = 'ocultar' then //NFe.Ocultar
           Ocultar1Click( FrmACBrMonitor )

        else if Cmd.Metodo = 'encerrarmonitor' then //NFe.EncerrarMonitor
           Application.Terminate

        else if Cmd.Metodo = 'ativo' then  //NFe.Ativo
           Cmd.Resposta := 'Ativo'

        else if Cmd.Metodo = 'versao' then //NFe.Versao
           Cmd.Resposta := sVersaoACBr

        else if Cmd.Metodo = 'versaonome' then //NFe.VersaoNome
           Cmd.Resposta := {$IFDEF ACBrNFeOpenSSL} 'OpenSSL' {$ELSE} 'CAPICOM' {$ENDIF}

        else if pos('|'+Cmd.Metodo+'|', '|exit|bye|fim|sair|') > 0 then {fecha conexao} //NFe.Exit
         begin
           Cmd.Resposta := 'Obrigado por usar o ACBrNFeMonitor' ;
           mCmd.Lines.Clear;

           if Assigned( Conexao ) then
             Conexao.CloseSocket ;
         end


        else //Else Final - Se chegou ate aqui, o comando é inválido
           raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;

     finally
         if wDiretorioAtual <> GetCurrentDir then
           SetCurrentDir(wDiretorioAtual);

         Cmd.Resposta := Cmd.Resposta + FrmACBrMonitor.RespostaIntegrador;
     end ;
  end;
end ;

function UFparaCodigo(const UF: string): integer;
const
  (**)UFS = '.AC.AL.AP.AM.BA.CE.DF.ES.GO.MA.MT.MS.MG.PA.PB.PR.PE.PI.RJ.RN.RS.RO.RR.SC.SP.SE.TO.';
  CODIGOS = '.12.27.16.13.29.23.53.32.52.21.51.50.31.15.25.41.26.22.33.24.43.11.14.42.35.28.17.';
begin
  try
    result := StrToInt(copy(CODIGOS, pos('.' + UF + '.', UFS) + 1, 2));
  except
    result := 0;
  end;
end;


function SubstituirVariaveis(const ATexto: String): String;
var
  TextoStr: String;
begin
  if Trim(ATexto) = '' then
    Result := ''
  else
  begin
    TextoStr := ATexto;

    if FrmACBrMonitor.ACBrNFe1.NotasFiscais.Count > 0 then
    begin
      with FrmACBrMonitor.ACBrNFe1.NotasFiscais.Items[0].NFe do
      begin
        TextoStr := StringReplace(TextoStr,'[EmitNome]',     Emit.xNome,   [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[EmitFantasia]', Emit.xFant,   [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[EmitCNPJCPF]',  Emit.CNPJCPF, [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[EmitIE]',       Emit.IE,      [rfReplaceAll, rfIgnoreCase]);

        TextoStr := StringReplace(TextoStr,'[DestNome]',     Dest.xNome,   [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[DestCNPJCPF]',  Dest.CNPJCPF, [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[DestIE]',       Dest.IE,      [rfReplaceAll, rfIgnoreCase]);

        TextoStr := StringReplace(TextoStr,'[ChaveNFe]',     procNFe.chNFe, [rfReplaceAll, rfIgnoreCase]);

        TextoStr := StringReplace(TextoStr,'[SerieNF]',      FormatFloat('000',           Ide.serie),         [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[NumeroNF]',     FormatFloat('000000000',     Ide.nNF),           [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[ValorNF]',      FormatFloat('0.00',          Total.ICMSTot.vNF), [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[dtEmissao]',    FormatDateTime('dd/mm/yyyy', Ide.dEmi),          [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[dtSaida]',      FormatDateTime('dd/mm/yyyy', Ide.dSaiEnt),       [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[hrSaida]',      FormatDateTime('hh:mm:ss',   Ide.hSaiEnt),       [rfReplaceAll, rfIgnoreCase]);
      end;
    end;
    Result := TextoStr;
  end;
end;


end.


