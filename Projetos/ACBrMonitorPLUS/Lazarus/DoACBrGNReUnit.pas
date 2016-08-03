{$I ACBr.inc}

unit DoACBRGNReUnit ;

interface

Uses  Classes, TypInfo, SysUtils, CmdUnit,  StdCtrls;
Procedure DoACBrGNRe( Cmd : TACBrCmd ) ;
Function ConvertStrRecived( AStr: String ) : String ;
procedure LerIniGuia(aStr: AnsiString);
implementation

Uses IniFiles, StrUtils, DateUtils, Forms,  ACBrUtil, ACBrMonitor1 ,  pcnConversao, pgnreConversao, ACBrGNREGuiaClass;

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
          if Cmd.Metodo = 'consultaconfig' then  //NFe.StatusServico
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
            'ambiente='                  +TpAmbToStr(ACBrGNRE1.WebServices.ConsultaUF.ambiente)+sLineBreak+
            'codigo='                    +IntToStr(ACBrGNRE1.WebServices.ConsultaUF.codigo)+sLineBreak+
            'descricao='                 +ACBrGNRE1.WebServices.ConsultaUF.descricao+sLineBreak+
            'Uf: '                       +ACBrGNRE1.WebServices.ConsultaUF.Uf+sLineBreak+
            'exigeUfFavorecida='         + IfThen(ACBrGNRE1.WebServices.ConsultaUF.exigeUfFavorecida = 'S', 'SIM', 'NÃO')+sLineBreak+
            'exigeReceita='              + IfThen(ACBrGNRE1.WebServices.ConsultaUF.exigeReceita = 'S', 'SIM', 'NÃO')+sLineBreak;
          end
          else if Cmd.Metodo = 'imprimirgnre' then //NFe.ImprimirDanfe(cArqXML,cImpressora,nNumCopias,cProtocolo,bMostrarPreview,cMarcaDaqgua,bViaConsumidor,bSimplificado)
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
            //ACBrGNRE1.GuiasRetorno.Clear;
            ACBrGNRE1.Enviar;
            Cmd.Resposta :=Cmd.Resposta + LineBreak+
            'Envio GNRE'+ LineBreak+
            'ambiente: '+ TpAmbToStr(ACBrGNRE1.WebServices.Retorno.ambiente)+ LineBreak+
            'codigo: '+ IntToStr(ACBrGNRE1.WebServices.Retorno.codigo)+ LineBreak+
            'descricao: '+ ACBrGNRE1.WebServices.Retorno.descricao+ LineBreak+
            'Recibo: '+ ACBrGNRE1.WebServices.Retorno.numeroRecibo+ LineBreak+
            'Protocolo: '+ ACBrGNRE1.WebServices.Retorno.protocolo+ LineBreak;

            //ACBrGNRE1.GuiasRetorno.LoadFromFile(ACBrGNRE1.Guias.Items[0].NomeArq);;
//            ACBrGNRE1.GuiasRetorno.ImprimirPDF;

//            ACBrGNRE1.GuiasRetorno.Items[0].ImprimirPDF;
            //ACBrGNRE1.Guias.ImprimirPDF;
            //ArqPDF := 'Guia-guia.xml';
            //ACBrGNRE1.Guias.;
            //ACBrGNRE1.GNREGuia := ACBrGNREGuiaRL1;
            //ACBrGNRE1.GuiasRetorno.ImprimirPDF;
            //ArqPDF := 'Guia-guia.pdf';
          end
          else
          raise Exception.Create(ACBrStr('Comando inválido ('+Cmd.Comando+')'));
       finally
          if wDiretorioAtual <> GetCurrentDir then
          SetCurrentDir(wDiretorioAtual);
       end;
     end;
   end;
function ConvertStrRecived(AStr: String): String;
 Var P   : Integer ;
     Hex : String ;
     CharHex : Char ;
begin
  { Verificando por codigos em Hexa }
  Result := AStr ;

  P := pos('\x',Result) ;
  while P > 0 do
  begin
     Hex := copy(Result,P+2,2) ;

     try
        CharHex := Chr(StrToInt('$'+Hex)) ;
     except
        CharHex := ' ' ;
     end ;

     Result := StringReplace(Result,'\x'+Hex,CharHex,[rfReplaceAll]) ;
     P      := pos('\x',Result) ;
  end ;
end ;

procedure LerIniGuia( aStr: AnsiString) ;
var
   IniGuia: TMemIniFile;
   SL: TStringList;
   ContTitulos: Integer;
   NomeSessao: String;
   begin
     IniGuia := TMemIniFile.Create('guia.ini');
     SL         := TStringList.Create;
     try
        try
        if (pos(#10,aStr) = 0) and FileExists(aStr) then
        SL.LoadFromFile(aStr)
        else
           SL.Text := ConvertStrRecived(aStr);
        IniGuia.SetStrings(SL);
        with FrmACBrMonitor.ACBrGNRE1 do
             begin
               Guias.Clear;
               with Guias.Add.GNRE do
                    begin
                       if IniGuia.SectionExists('Emitente') then
                       begin
                       c27_tipoIdentificacaoEmitente       :=IniGuia.ReadInteger('Emitente','tipo',0);//[1,2] INscrito na uf ou nao /////1 CNPJ - 2 CPF
                       //Se inscrito na UF Destino
                       c17_inscricaoEstadualEmitente         :=IniGuia.ReadString('Emitente','IE','');  //IE inscrito na uf destino
                       //Se nao Inscrito na uf Destino
                       c03_idContribuinteEmitente            :=IniGuia.ReadString('Emitente','id','cnpjcpf'); //numero do cnpj ou cpf
                       c16_razaoSocialEmitente               :=IniGuia.ReadString('Emitente','RazaoSocial','nome');
                       c18_enderecoEmitente                  :=IniGuia.ReadString('Emitente','Endereco','');
                       c19_municipioEmitente                 :=IniGuia.ReadString('Emitente','Cidade','');
                       c20_ufEnderecoEmitente                :=IniGuia.ReadString('Emitente','UF','');
                       c21_cepEmitente                       :=IniGuia.ReadString('Emitente','Cep','');
                       c22_telefoneEmitente                  :=IniGuia.ReadString('Emitente','Telefone','');
                       end;
                       //Complementes da Recita
                       if IniGuia.SectionExists('Complemento') then
                       begin
                       c42_identificadorGuia  :=IniGuia.ReadString('Complemento','IdenfiticadorGuia','');
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
                       camposExtras.Add.CampoExtra.codigo  :=IniGuia.ReadInteger('CampoExtra','codigo',0);
                       camposExtras.Add.CampoExtra.tipo    :=IniGuia.ReadString('CampoExtra','tipo','');
                       camposExtras.Add.CampoExtra.valor   :=IniGuia.ReadString('CampoExtra','valor','');
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
        SL.Free;
        IniGuia.Free;
     end;
   end;
end.
