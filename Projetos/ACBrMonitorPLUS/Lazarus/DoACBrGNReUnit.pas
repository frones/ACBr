{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Alberto Leal                                    }
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
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}
{$I ACBr.inc}

unit DoACBrGNReUnit;

interface

Uses Classes, TypInfo, SysUtils, CmdUnit, StdCtrls, DoACBrUnit;

procedure DoACBrGNRe( Cmd : TACBrCmd ) ;
procedure LerIniGuia(aStr: AnsiString);

implementation

Uses IniFiles, StrUtils, DateUtils, Forms,  ACBrUtil, ACBrMonitor1, pcnConversao,
  pgnreConversao, ACBrGNREGuiaClass;

procedure DoACBrGNRe ( Cmd: TACBrCmd ) ;
var
  wDiretorioAtual : String;
  Salva, OK, bImprimir, bMostrarPreview, bImprimirPDF : Boolean;
  ArqNFe, ArqPDF, ArqEvento, Chave, cImpressora : String;
begin
  with FrmACBrMonitor do
  begin
    wDiretorioAtual := GetCurrentDir;
    try
      if Cmd.Metodo = 'consultaconfig' then
      begin
        ACBrGNRE1.WebServices.ConsultaUF.Uf := Cmd.Params(0);
        ACBrGNRE1.WebServices.ConsultaUF.receita := StrToIntDef(Cmd.Params(1), 0);
        try
          ACBrGNRE1.WebServices.ConsultaUF.Executar;
        except
        on E: Exception do
          begin
            raise Exception.Create(ACBrGNRE1.WebServices.Enviar.Msg+sLineBreak+E.Message);
          end;
        end;

        Cmd.Resposta := ACBrGNRE1.WebServices.Enviar.Msg+
                     '[STATUS]'+sLineBreak+
                     'Ambiente='+TpAmbToStr(ACBrGNRE1.WebServices.ConsultaUF.ambiente)+sLineBreak+
                     'Codigo='+IntToStr(ACBrGNRE1.WebServices.ConsultaUF.codigo)+sLineBreak+
                     'Descricao='+ACBrGNRE1.WebServices.ConsultaUF.descricao+sLineBreak+
                     'UF='+ACBrGNRE1.WebServices.ConsultaUF.Uf+sLineBreak+
                     'AxigeUfFavorecida='+IfThen(ACBrGNRE1.WebServices.ConsultaUF.exigeUfFavorecida = 'S', 'SIM', 'NÃO')+sLineBreak+
                     'AxigeReceita='+IfThen(ACBrGNRE1.WebServices.ConsultaUF.exigeReceita = 'S', 'SIM', 'NÃO')+sLineBreak;
      end
      else if Cmd.Metodo = 'imprimirgnre' then
      begin
        begin
          ACBrGNRE1.GuiasRetorno.Clear;
           if FileExists(Cmd.Params(0)) or
             FileExists(PathWithDelim(ACBrGNRE1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)) or
             FileExists(PathWithDelim(ACBrGNRE1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)+'-gnre.txt') then
           begin
              if FileExists(Cmd.Params(0)) then
                ACBrGNRE1.GuiasRetorno.LoadFromFile(Cmd.Params(0))
              else if FileExists(PathWithDelim(ACBrGNRE1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)) then
                ACBrGNRE1.GuiasRetorno.LoadFromFile(PathWithDelim(ACBrGNRE1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0))
              else
                ACBrGNRE1.GuiasRetorno.LoadFromFile(PathWithDelim(ACBrNFe1.Configuracoes.Arquivos.PathSalvar)+Cmd.Params(0)+'-gnre.txt');
           end
           else
           raise Exception.Create('Arquivo '+Cmd.Params(0)+' não encontrado.');
           bMostrarPreview := (Cmd.Params(4) = '1');
           if NaoEstaVazio(Cmd.Params(1)) then
             ACBrGNRE1.GNREGuia.Impressora:= Cmd.Params(1);

           if NaoEstaVazio(Cmd.Params(2)) then
             ACBrGNRE1.GNREGuia.NumCopias :=StrToIntDef(Cmd.Params(2),1);

           ACBrGNRE1.GuiasRetorno.Imprimir;
           Cmd.Resposta := 'Guia GNRe Impressa com sucesso';

           if ACBrGNRE1.GNREGuia.MostrarPreview then
             Ocultar1.Click;
        end
      end
      else if Cmd.Metodo = 'imprimirgnrepdf' then //NFe.ImprimirDANFEPDF(cArqXML,cProtocolo,cMarcaDaqgua,bViaConsumidor,bSimplificado)
      begin
       ACBrGNRE1.GuiasRetorno.Clear;
         if FileExists(Cmd.Params(0)) then
           ACBrGNRE1.GuiasRetorno.LoadFromFile(Cmd.Params(0))
         else
            raise Exception.Create('Arquivo '+Cmd.Params(0)+' não encontrado.');
         try
           ACBrGNRE1.GuiasRetorno.ImprimirPDF;
           ArqPDF := 'GNRE_' +ACBrGNRE1.GuiasRetorno.Items[0].GNRE.RepresentacaoNumerica+'.pdf';
           Cmd.Resposta := 'Arquivo criado em: '+ PathWithDelim(ACBrGNRE1.GNREGuia.PathPDF) + ArqPDF ;
         except
           raise Exception.Create('Erro ao criar o arquivo PDF');
         end;
      end
      else if cmd.Metodo = 'gerarguia' then
      begin
        LerIniGuia(Cmd.Params(0));
        ACBrGNRE1.Guias.GerarGNRE;
        ACBrGNRE1.Guias.Items[0].GravarXML;
        Cmd.Resposta:= 'Arquivo gerado em: '+ACBrGNRE1.Guias.Items[0].NomeArq;

        ACBrGNRE1.Enviar;

        Cmd.Resposta :=Cmd.Resposta + sLineBreak+
                     'Envio GNRE'+ sLineBreak+
                     'ambiente: '+ TpAmbToStr(ACBrGNRE1.WebServices.Retorno.ambiente)+ sLineBreak+
                     'codigo: '+ IntToStr(ACBrGNRE1.WebServices.Retorno.codigo)+ sLineBreak+
                     'descricao: '+ ACBrGNRE1.WebServices.Retorno.descricao+ sLineBreak+
                     'Recibo: '+ ACBrGNRE1.WebServices.Retorno.numeroRecibo+ sLineBreak+
                     'Protocolo: '+ ACBrGNRE1.WebServices.Retorno.protocolo+ sLineBreak;

      end
      else if Cmd.Metodo = 'setformaemissao' then
      begin
         if cbModoEmissao.checked then
             exit;

         if (StrToInt(Cmd.Params(0))>=1) and (StrToInt(Cmd.Params(0))<=9) then
          begin
            ACBrGNRe1.Configuracoes.Geral.FormaEmissao := StrToTpEmis(OK, Cmd.Params(0));
            cbFormaEmissaoGNRe.ItemIndex := ACBrGNRE1.Configuracoes.Geral.FormaEmissaoCodigo-1;
            SalvarIni;
          end
         else
            raise Exception.Create('Forma de Emissão Inválida.');
      end

      else
      raise Exception.Create(ACBrStr('Comando inválido ('+Cmd.Comando+')'));
    finally
      if wDiretorioAtual <> GetCurrentDir then
      SetCurrentDir(wDiretorioAtual);
    end;
  end;
end;

procedure LerIniGuia( aStr: AnsiString) ;
var
  IniGuia: TMemIniFile;
begin
  IniGuia := LerConverterIni(aStr);

  try
    try
      with FrmACBrMonitor.ACBrGNRE1 do
      begin
        Guias.Clear;
        with Guias.Add.GNRE do
        begin
          if IniGuia.SectionExists('Emitente') then
          begin
            c27_tipoIdentificacaoEmitente :=IniGuia.ReadInteger('Emitente','tipo',0);//[1,2] INscrito na uf ou nao /////1 CNPJ - 2 CPF
            //Se inscrito na UF Destino
            c17_inscricaoEstadualEmitente :=IniGuia.ReadString('Emitente','IE','');  //IE inscrito na uf destino
            //Se nao Inscrito na uf Destino
            c03_idContribuinteEmitente    :=IniGuia.ReadString('Emitente','id','cnpjcpf'); //numero do cnpj ou cpf
            c16_razaoSocialEmitente       :=IniGuia.ReadString('Emitente','RazaoSocial','nome');
            c18_enderecoEmitente          :=IniGuia.ReadString('Emitente','Endereco','');
            c19_municipioEmitente         :=IniGuia.ReadString('Emitente','Cidade','');
            c20_ufEnderecoEmitente        :=IniGuia.ReadString('Emitente','UF','');
            c21_cepEmitente               :=IniGuia.ReadString('Emitente','Cep','');
            c22_telefoneEmitente          :=IniGuia.ReadString('Emitente','Telefone','');
          end;

          //Complementes da Recita
          if IniGuia.SectionExists('Complemento') then
          begin
            c42_identificadorGuia  :=IniGuia.ReadString('Complemento','IdentificadorGuia','');
            ///Exige Doc Origem
            c28_tipoDocOrigem      :=IniGuia.ReadInteger('Complemento','tipoDocOrigem',0);
            c04_docOrigem          :=IniGuia.ReadString('Complemento','DocOrigem','');
            ///Exige Detalhamento Receita
            c25_detalhamentoReceita :=IniGuia.ReadInteger('Complemento','detalhamentoReceita',0);
            ///Exige Produto
            c26_produto             :=IniGuia.ReadInteger('Complemento','produto',0);
          end;

          //Referencias Da Receita
          if IniGuia.SectionExists('Referencia') then
          begin
            c15_convenio       :=IniGuia.ReadString('Referencia','convenio','');
            c02_receita        :=IniGuia.ReadInteger('Referencia','receita',0);
            c01_UfFavorecida   :=IniGuia.ReadString('Referencia','ufFavorecida','');
            c14_dataVencimento :=StringToDateTime(IniGuia.ReadString('Referencia','dataVencimento',''));
            c33_dataPagamento  :=StringToDateTime(IniGuia.ReadString('Referencia','dataPagamento',''));
            referencia.ano     :=IniGuia.ReadInteger('Referencia','referenciaAno',0);
            referencia.mes     :=IniGuia.ReadString('Referencia','referenciaMes','');
            referencia.parcela :=IniGuia.ReadInteger('Referencia','referenciaParcela',1);
            referencia.periodo :=IniGuia.ReadInteger('Referencia','referenciaPeriodo',0);
            c10_valorTotal     :=StringToFloatDef(IniGuia.ReadString('Referencia','ValorTotal',''),0);
            c06_valorPrincipal :=StringToFloatDef(IniGuia.ReadString('Referencia','ValorPrincipal',''),0);
          end;

          //Destinatario
          if IniGuia.SectionExists('Destinatario') then
          begin
            c34_tipoIdentificacaoDestinatario :=IniGuia.ReadInteger('Destinatario','tipo',0);/// 1 CNPJ - 2 CPF
            //Se inscrito
            c36_inscricaoEstadualDestinatario :=IniGuia.ReadString('Destinatario','ie','');
            //Se nao inscrito
            c35_idContribuinteDestinatario    :=IniGuia.ReadString('Destinatario','id','cnpjcpf');
            c37_razaoSocialDestinatario       :=IniGuia.ReadString('Destinatario','razaosocial','nome');
            c38_municipioDestinatario         :=IniGuia.ReadString('Destinatario','cidade','');
          end;

          //Outras Informacoes
          if IniGuia.SectionExists('CampoExtra') then
          begin
            camposExtras.Clear;
            with camposExtras.Add do
            begin
                 CampoExtra.codigo  :=IniGuia.ReadInteger('CampoExtra','codigo',0);
                 CampoExtra.tipo    :=IniGuia.ReadString('CampoExtra','tipo','');
                 CampoExtra.valor   :=IniGuia.ReadString('CampoExtra','valor','');
            end;
          end;
        end;
      end;
  except
    on E: Exception do
    begin
      raise Exception.Create('Falha ao criar guia'+sLineBreak+E.Message);
    end;
  end;
  finally
    IniGuia.Free;
  end;
end;
end.
