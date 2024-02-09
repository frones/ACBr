{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

{$I ACBr.inc}

unit pcnReinfRetConsulta;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  pcnConversao, pcnLeitor,
  pcnCommonReinf, pcnConversaoReinf, pcnReinfRetConsulta_R9011;

type
  TRetConsulta_R5011 = class(TObject)
  private
    FLeitor: TLeitor;
    FevtTotalContrib: TEvtTotalContrib;
    FXML: String;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: boolean;
    function SalvarINI: boolean;

    property Leitor: TLeitor read FLeitor write FLeitor;
    property evtTotalContrib: TEvtTotalContrib read FevtTotalContrib write FevtTotalContrib;
    property XML: String read FXML;
  end;

  TRetConsulta = class(TRetConsulta_R5011); // Remover após entrar em vigor a versão 2_01_01

implementation

uses
  IniFiles;

{ TRetConsulta_R5011 }

constructor TRetConsulta_R5011.Create;
begin
  FLeitor := TLeitor.Create;
  FevtTotalContrib := TEvtTotalContrib.Create;
end;

destructor TRetConsulta_R5011.Destroy;
begin
  FLeitor.Free;
  FevtTotalContrib.Free;

  inherited;
end;

function TRetConsulta_R5011.LerXml: boolean;
var
  i, j: Integer;
  Ok: Boolean;
begin
  Result := True;
  try
    Leitor.Grupo := Leitor.Arquivo;

    FXML := Leitor.Arquivo;

    if (leitor.rExtrai(1, 'evtTotalContrib') <> '') then
    begin
      with evtTotalContrib do
      begin
        Id := Leitor.rAtributo('id=');

        if leitor.rExtrai(2, 'ideEvento') <> '' then
          IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');

        if leitor.rExtrai(2, 'ideContri') <> '' then
        begin
          IdeContri.TpInsc := StrToTpInscricao(Ok, leitor.rCampo(tcStr, 'tpInsc'));
          IdeContri.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
        end;

        if leitor.rExtrai(2, 'ideRecRetorno') <> '' then
        begin
          if leitor.rExtrai(3, 'ideStatus') <> '' then
          begin
            IdeStatus.cdRetorno   := leitor.rCampo(tcStr, 'cdRetorno');
            IdeStatus.descRetorno := leitor.rCampo(tcStr, 'descRetorno');

            i := 0;
            while Leitor.rExtrai(4, 'regOcorrs', '', i + 1) <> '' do
            begin
              IdeStatus.regOcorrs.New;
              IdeStatus.regOcorrs.Items[i].tpOcorr        := leitor.rCampo(tcInt, 'tpOcorr');
              IdeStatus.regOcorrs.Items[i].localErroAviso := leitor.rCampo(tcStr, 'localErroAviso');
              IdeStatus.regOcorrs.Items[i].codResp        := leitor.rCampo(tcStr, 'codResp');
              IdeStatus.regOcorrs.Items[i].dscResp        := leitor.rCampo(tcStr, 'dscResp');

              inc(i);
            end;
          end;
        end;

        if leitor.rExtrai(2, 'infoRecEv') <> '' then
        begin
          infoRecEv.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
          infoRecEv.nrProtEntr := leitor.rCampo(tcStr, 'nrProtEntr');
          infoRecEv.dhProcess  := leitor.rCampo(tcDatHor, 'dhProcess');
          infoRecEv.dhRecepcao := leitor.rCampo(tcDatHor, 'dhRecepcao');
          infoRecEv.tpEv       := leitor.rCampo(tcStr, 'tpEv');
          infoRecEv.idEv       := leitor.rCampo(tcStr, 'idEv');
          infoRecEv.hash       := leitor.rCampo(tcStr, 'hash');
        end;

        if leitor.rExtrai(2, 'infoTotalContrib') <> '' then
        begin
          with infoTotalContrib do
          begin
            nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
            indExistInfo := StrToindExistInfo(Ok, leitor.rCampo(tcStr, 'indExistInfo'));

            i := 0;
            while Leitor.rExtrai(3, 'RTom', '', i + 1) <> '' do
            begin
              RTom.New;

              RTom.Items[i].cnpjPrestador     := leitor.rCampo(tcStr, 'cnpjPrestador');
              RTom.Items[i].cno               := leitor.rCampo(tcStr, 'cno');
              RTom.Items[i].vlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
              RTom.Items[i].vlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
              RTom.Items[i].vlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
              RTom.Items[i].vlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
              RTom.Items[i].vlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');

              // Versão 1.03.02
              j := 0;
              while Leitor.rExtrai(4, 'infoCRTom', '', j + 1) <> '' do
              begin
                RTom.Items[i].infoCRTom.New;

                RTom.Items[i].infoCRTom.Items[j].CRTom        := leitor.rCampo(tcStr, 'CRTom');
                RTom.Items[i].infoCRTom.Items[j].VlrCRTom     := leitor.rCampo(tcDe2, 'VlrCRTom');
                RTom.Items[i].infoCRTom.Items[j].VlrCRTomSusp := leitor.rCampo(tcDe2, 'VlrCRTomSusp');

                inc(j);
              end;

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'RPrest', '', i + 1) <> '' do
            begin
              RPrest.New;

              RPrest.Items[i].tpInscTomador     := StrToTpInscricao(Ok, leitor.rCampo(tcStr, 'tpInscTomador'));
              RPrest.Items[i].nrInscTomador     := leitor.rCampo(tcStr, 'nrInscTomador');
              RPrest.Items[i].vlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
              RPrest.Items[i].vlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
              RPrest.Items[i].vlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
              RPrest.Items[i].vlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
              RPrest.Items[i].vlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'RRecRepAD', '', i + 1) <> '' do
            begin
              RRecRepAD.New;

              RRecRepAD.Items[i].cnpjAssocDesp := leitor.rCampo(tcStr, 'cnpjAssocDesp');
              RRecRepAD.Items[i].vlrTotalRep   := leitor.rCampo(tcDe2, 'vlrTotalRep');
              RRecRepAD.Items[i].vlrTotalRet   := leitor.rCampo(tcDe2, 'vlrTotalRet');
              RRecRepAD.Items[i].vlrTotalNRet  := leitor.rCampo(tcDe2, 'vlrTotalNRet');

              // Versão 1.03.02
              RRecRepAD.Items[i].CRRecRepAD        := leitor.rCampo(tcStr, 'CRRecRepAD');
              RRecRepAD.Items[i].vlrCRRecRepAD     := leitor.rCampo(tcDe2, 'vlrCRRecRepAD');
              RRecRepAD.Items[i].vlrCRRecRepADSusp := leitor.rCampo(tcDe2, 'vlrCRRecRepADSusp');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'RComl', '', i + 1) <> '' do
            begin
              RComl.New;
              RComl.Items[i].vlrCPApur    := leitor.rCampo(tcDe2, 'vlrCPApur');
              RComl.Items[i].vlrRatApur   := leitor.rCampo(tcDe2, 'vlrRatApur');
              RComl.Items[i].vlrSenarApur := leitor.rCampo(tcDe2, 'vlrSenarApur');
              RComl.Items[i].vlrCPSusp    := leitor.rCampo(tcDe2, 'vlrCPSusp');
              RComl.Items[i].vlrRatSusp   := leitor.rCampo(tcDe2, 'vlrRatSusp');
              RComl.Items[i].vlrSenarSusp := leitor.rCampo(tcDe2, 'vlrSenarSusp');

              // Versão 1.03.02
              RComl.Items[i].CRComl        := leitor.rCampo(tcStr, 'CRComl');
              RComl.Items[i].vlrCRComl     := leitor.rCampo(tcDe2, 'vlrCRComl');
              RComl.Items[i].vlrCRComlSusp := leitor.rCampo(tcDe2, 'vlrCRComlSusp');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'RAquis', '', i + 1) <> '' do
            begin
              RAquis.New;

              RAquis.Items[i].CRAquis         := leitor.rCampo(tcStr, 'CRAquis');
              RAquis.Items[i].vlrCRAquis      := leitor.rCampo(tcDe2, 'vlrCRAquis');
              RAquis.Items[i].vlrCRAquisSusp  := leitor.rCampo(tcDe2, 'vlrCRAquisSusp');

              inc(i);
            end;

            i := 0;
            while Leitor.rExtrai(3, 'RCPRB', '', i + 1) <> '' do
            begin
              RCPRB.New;

              RCPRB.Items[i].codRec         := leitor.rCampo(tcInt, 'codRec');
              RCPRB.Items[i].vlrCPApurTotal := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
              RCPRB.Items[i].vlrCPRBSusp    := leitor.rCampo(tcDe2, 'vlrCPRBSusp');

              // Versão 1.03.02
              RCPRB.Items[i].CRCPRB        := leitor.rCampo(tcStr, 'CRCPRB');
              RCPRB.Items[i].vlrCRCPRB     := leitor.rCampo(tcDe2, 'vlrCRCPRB');
              RCPRB.Items[i].vlrCRCPRBSusp := leitor.rCampo(tcDe2, 'vlrCRCPRBSusp');

              inc(i);
            end;
          end;
        end;
      end;
    end
    else
    if (leitor.rExtrai(1, 'Reinf') <> '') then
    begin
      with evtTotalContrib do
      begin
        if leitor.rExtrai(2, 'ideStatus') <> '' then
        begin
          IdeStatus.cdRetorno   := leitor.rCampo(tcStr, 'cdRetorno');
          IdeStatus.descRetorno := leitor.rCampo(tcStr, 'descRetorno');

          i := 0;
          while Leitor.rExtrai(3, 'regOcorrs', '', i + 1) <> '' do
          begin
            IdeStatus.regOcorrs.New;
            IdeStatus.regOcorrs.Items[i].tpOcorr        := leitor.rCampo(tcInt, 'tpOcorr');
            IdeStatus.regOcorrs.Items[i].localErroAviso := leitor.rCampo(tcStr, 'localErroAviso');
            IdeStatus.regOcorrs.Items[i].codResp        := leitor.rCampo(tcStr, 'codResp');
            IdeStatus.regOcorrs.Items[i].dscResp        := leitor.rCampo(tcStr, 'dscResp');

            inc(i);
          end;
        end;

        if leitor.rExtrai(2, 'retornoEventos') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(3, 'evento', '', i + 1) <> '' do
          begin
            with RetornoEventos.New do
            begin
              id                := Leitor.rAtributo('id=');
              iniValid          := leitor.rCampo(tcStr, 'iniValid');
              fimValid          := leitor.rCampo(tcStr, 'fimValid');
              dtHoraRecebimento := leitor.rCampo(tcStr, 'dtHoraRecebimento');
              nrProtocolo       := leitor.rCampo(tcStr, 'nrProtocolo');
              nrRecibo          := leitor.rCampo(tcStr, 'nrRecibo');
              situacaoEvento    := leitor.rCampo(tcStr, 'situacaoEvento');
              aplicacaoRecepcao := leitor.rCampo(tcStr, 'aplicacaoRecepcao');

              inc(i);
            end;
          end;
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function TRetConsulta_R5011.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i, j: Integer;
begin
  Result := True;

  AIni := TMemIniFile.Create('');
  try
    with Self do
    begin
      with evtTotalContrib do
      begin
        sSecao := 'evtTotalContrib';
        AIni.WriteString(sSecao, 'Id', Id);

        sSecao := 'ideEvento';
        AIni.WriteString(sSecao, 'perApur', IdeEvento.perApur);

        sSecao := 'ideContri';
        AIni.WriteString(sSecao, 'tpInsc', TpInscricaoToStr(IdeContri.TpInsc));
        AIni.WriteString(sSecao, 'nrInsc', IdeContri.nrInsc);

        sSecao := 'ideStatus';
        AIni.WriteString(sSecao, 'cdRetorno', ideStatus.cdRetorno);
        AIni.WriteString(sSecao, 'descRetorno', ideStatus.descRetorno);

        for i := 0 to ideStatus.regOcorrs.Count -1 do
        begin
          sSecao := 'regOcorrs' + IntToStrZero(I, 3);

          AIni.WriteInteger(sSecao, 'tpOcorr',       ideStatus.regOcorrs.Items[i].tpOcorr);
          AIni.WriteString(sSecao, 'localErroAviso', ideStatus.regOcorrs.Items[i].localErroAviso);
          AIni.WriteString(sSecao, 'codResp',        ideStatus.regOcorrs.Items[i].codResp);
          AIni.WriteString(sSecao, 'dscResp',        ideStatus.regOcorrs.Items[i].dscResp);
        end;

        sSecao := 'infoRecEv';
        AIni.WriteString(sSecao, 'nrRecArqBase', infoRecEv.nrRecArqBase);
        AIni.WriteString(sSecao, 'nrProtEntr', infoRecEv.nrProtEntr);
        AIni.WriteString(sSecao, 'dhProcess',  DateToStr(infoRecEv.dhProcess));
        AIni.WriteString(sSecao, 'dhRecepcao', DateToStr(infoRecEv.dhRecepcao));
        AIni.WriteString(sSecao, 'tpEv',       infoRecEv.tpEv);
        AIni.WriteString(sSecao, 'idEv',       infoRecEv.idEv);
        AIni.WriteString(sSecao, 'hash',       infoRecEv.hash);

        sSecao := 'infoTotalContrib';
        AIni.WriteString(sSecao, 'nrRecArqBase', infoTotalContrib.nrRecArqBase);
        AIni.WriteString(sSecao, 'indExistInfo', indExistInfoToStr(infoTotalContrib.indExistInfo));

        with infoTotalContrib do
        begin
          for i := 0 to RTom.Count -1 do
          begin
            sSecao := 'RTom' + IntToStrZero(I, 3);

            AIni.WriteString(sSecao, 'cnpjPrestador',    RTom.Items[i].cnpjPrestador);
            AIni.WriteString(sSecao, 'cno',              RTom.Items[i].cno);
            AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   RTom.Items[i].vlrTotalBaseRet);
            AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  RTom.Items[i].vlrTotalRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   RTom.Items[i].vlrTotalRetAdic);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', RTom.Items[i].vlrTotalNRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  RTom.Items[i].vlrTotalNRetAdic);

            // Versão 1.03.02
            for j := 0 to RTom.Items[i].infoCRTom.Count -1 do
            begin
              sSecao := 'infoCRTom' + IntToStrZero(I, 3) + IntToStrZero(J, 1);

              AIni.WriteString(sSecao, 'CRTom',    RTom.Items[i].infoCRTom.Items[J].CRTom);
              AIni.WriteFloat(sSecao, 'VlrCRTom',   RTom.Items[i].infoCRTom.Items[J].VlrCRTom);
              AIni.WriteFloat(sSecao, 'VlrCRTomSusp',  RTom.Items[i].infoCRTom.Items[J].VlrCRTomSusp);
            end;
          end;

          for i := 0 to RPrest.Count -1 do
          begin
            sSecao := 'RPrest' + IntToStrZero(I, 3);

            AIni.WriteString(sSecao, 'tpInscTomador',    TpInscricaoToStr(RPrest.Items[i].tpInscTomador));
            AIni.WriteString(sSecao, 'nrInscTomador',    RPrest.Items[i].nrInscTomador);
            AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   RPrest.Items[i].vlrTotalBaseRet);
            AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  RPrest.Items[i].vlrTotalRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   RPrest.Items[i].vlrTotalRetAdic);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', RPrest.Items[i].vlrTotalNRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  RPrest.Items[i].vlrTotalNRetAdic);
          end;

          for i := 0 to RRecRepAD.Count -1 do
          begin
            sSecao := 'RRecRepAD' + IntToStrZero(I, 3);

            AIni.WriteString(sSecao, 'cnpjAssocDesp', RRecRepAD.Items[i].cnpjAssocDesp);
            AIni.WriteFloat(sSecao, 'vlrTotalRep',    RRecRepAD.Items[i].vlrTotalRep);
            AIni.WriteFloat(sSecao, 'vlrTotalRet',    RRecRepAD.Items[i].vlrTotalRet);
            AIni.WriteFloat(sSecao, 'vlrTotalNRet',   RRecRepAD.Items[i].vlrTotalNRet);
          end;

          for i := 0 to RComl.Count -1 do
          begin
            sSecao := 'RComl' + IntToStrZero(I, 1);

            AIni.WriteFloat(sSecao, 'vlrCPApur',    RComl.Items[i].vlrCPApur);
            AIni.WriteFloat(sSecao, 'vlrRatApur',   RComl.Items[i].vlrRatApur);
            AIni.WriteFloat(sSecao, 'vlrSenarApur', RComl.Items[i].vlrSenarApur);
            AIni.WriteFloat(sSecao, 'vlrCPSusp',    RComl.Items[i].vlrCPSusp);
            AIni.WriteFloat(sSecao, 'vlrRatSusp',   RComl.Items[i].vlrRatSusp);
            AIni.WriteFloat(sSecao, 'vlrSenarSusp', RComl.Items[i].vlrSenarSusp);

            // Versão 1.03.02
            AIni.WriteString(sSecao, 'CRComl',       RComl.Items[i].CRComl);
            AIni.WriteFloat(sSecao, 'vlrCRComl',     RComl.Items[i].vlrCRComl);
            AIni.WriteFloat(sSecao, 'vlrCRComlSusp', RComl.Items[i].vlrCRComlSusp);
          end;

          for i := 0 to RAquis.Count -1 do
          begin
            sSecao := 'RAquis' + IntToStrZero(I, 1);

            AIni.WriteString(sSecao,  'CRAquis',        RAquis.Items[i].CRAquis);
            AIni.WriteFloat(sSecao,   'vlrCRAquis',     RAquis.Items[i].vlrCRAquis);
            AIni.WriteFloat(sSecao,   'vlrCRAquisSusp', RAquis.Items[i].vlrCRAquisSusp);
          end;

          for i := 0 to RCPRB.Count -1 do
          begin
            sSecao := 'RCPRB' + IntToStrZero(I, 1);

            AIni.WriteInteger(sSecao, 'codRec',       RCPRB.Items[i].codRec);
            AIni.WriteFloat(sSecao, 'vlrCPApurTotal', RCPRB.Items[i].vlrCPApurTotal);
            AIni.WriteFloat(sSecao, 'vlrCPRBSusp',    RCPRB.Items[i].vlrCPRBSusp);

            // Versão 1.03.02
            AIni.WriteString(sSecao, 'CRCPRB',       RCPRB.Items[i].CRCPRB);
            AIni.WriteFloat(sSecao, 'vlrCRCPRB',     RCPRB.Items[i].vlrCRCPRB);
            AIni.WriteFloat(sSecao, 'vlrCRCPRBSusp', RCPRB.Items[i].vlrCRCPRBSusp);
          end;
        end;
      end;
    end;
  finally
    AIni.Free;
  end;
end;

end.
