{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pmdfeMDFeR;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnConversao, pcnLeitor,
  pmdfeConversaoMDFe, pmdfeMDFe,
  ACBrUtil.Strings,
  ACBrUtil.Base;

type

  TMDFeR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FMDFe: TMDFe;
  public
    constructor Create(AOwner: TMDFe);
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property MDFe: TMDFe     read FMDFe   write FMDFe;
  end;

implementation

{ TMDFeR }

constructor TMDFeR.Create(AOwner: TMDFe);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FMDFe := AOwner;
end;

destructor TMDFeR.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TMDFeR.LerXml: Boolean;
var
  ok: Boolean;
  i01, i02, i03, i04, i05: Integer;
  // as variáveis abaixo são utilizadas para identificar as várias ocorrências
  // da tag qtdRat.
  sAux: String;
  pos1, pos2, pos3: Integer;
  qtdRat_UnidTransp: Currency;
begin
  Leitor.Grupo := Leitor.Arquivo;

  MDFe.infMDFe.ID := Leitor.rAtributo('Id=', 'infMDFe');

  if OnlyNumber(MDFe.infMDFe.ID) = '' then
    raise Exception.Create('Não encontrei o atributo: Id');

  MDFe.infMDFe.versao := StringToFloatDef(Leitor.rAtributo('versao=', 'infMDFe'), -1);

  if MDFe.infMDFe.versao = -1 then
    raise Exception.Create('Não encontrei o atributo: versao');

  (* Grupo da TAG <ide> *******************************************************)
  if Leitor.rExtrai(1, 'ide') <> '' then
  begin
    MDFe.Ide.cUF         := Leitor.rCampo(tcInt, 'cUF');
    MDFe.Ide.tpAmb       := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    MDFe.Ide.tpEmit      := StrToTpEmitente(ok, Leitor.rCampo(tcStr, 'tpEmit'));
    if MDFe.infMDFe.versao >= 3 then
      MDFe.Ide.tpTransp := StrToTTransportador(ok, Leitor.rCampo(tcStr, 'tpTransp'));
    MDFe.Ide.modelo      := Leitor.rCampo(tcStr, 'mod');
    MDFe.Ide.serie       := Leitor.rCampo(tcInt, 'serie');
    MDFe.Ide.nMDF        := Leitor.rCampo(tcInt, 'nMDF');
    MDFe.Ide.cMDF        := Leitor.rCampo(tcStr, 'cMDF');
    MDFe.Ide.cDV         := Leitor.rCampo(tcInt, 'cDV');
    MDFe.Ide.modal       := StrToModal(ok, Leitor.rCampo(tcStr, 'modal'));
    MDFe.Ide.dhEmi       := Leitor.rCampo(tcDatHor, 'dhEmi');
    MDFe.Ide.tpEmis      := StrToTpEmis(ok, Leitor.rCampo(tcStr, 'tpEmis'));
    MDFe.Ide.procEmi     := StrToprocEmi(ok, Leitor.rCampo(tcStr, 'procEmi'));
    MDFe.Ide.verProc     := Leitor.rCampo(tcStr, 'verProc');
    MDFe.Ide.UFIni       := Leitor.rCampo(tcStr, 'UFIni');
    MDFe.Ide.UFFim       := Leitor.rCampo(tcStr, 'UFFim');
    MDFe.Ide.dhIniViagem := Leitor.rCampo(tcDatHor, 'dhIniViagem');

    if MDFe.infMDFe.versao >= 3 then
    begin
      if Leitor.rCampo(tcStr, 'indCanalVerde') = '1' then
        MDFe.ide.indCanalVerde := tiSim
      else
        MDFe.ide.indCanalVerde := tiNao;

      if Leitor.rCampo(tcStr, 'indCarregaPosterior') = '1' then
        MDFe.ide.indCarregaPosterior := tiSim
      else
        MDFe.ide.indCarregaPosterior := tiNao;
    end;

    i01 := 0;
    MDFe.Ide.infMunCarrega.Clear;
    while Leitor.rExtrai(2, 'infMunCarrega', '', i01 + 1) <> '' do
    begin
      MDFe.Ide.infMunCarrega.New;
      MDFe.Ide.infMunCarrega[i01].cMunCarrega := Leitor.rCampo(tcInt, 'cMunCarrega');
      MDFe.Ide.infMunCarrega[i01].xMunCarrega := Leitor.rCampo(tcStr, 'xMunCarrega');
      inc(i01);
    end;

    i01 := 0;
    MDFe.Ide.infPercurso.Clear;
    while Leitor.rExtrai(2, 'infPercurso', '', i01 + 1) <> '' do
    begin
      MDFe.Ide.infPercurso.New;
      MDFe.Ide.infPercurso[i01].UFPer := Leitor.rCampo(tcStr, 'UFPer');
      inc(i01);
    end;

  end;

  (* Grupo da TAG <emit> ******************************************************)
  if Leitor.rExtrai(1, 'emit') <> '' then
  begin
    MDFe.emit.CNPJCPF :=  Leitor.rCampoCNPJCPF;
//    MDFe.emit.CNPJ  := Leitor.rCampo(tcStr, 'CNPJ');
    MDFe.emit.IE      := Leitor.rCampo(tcStr, 'IE');
    MDFe.emit.xNome   := Leitor.rCampo(tcStr, 'xNome');
    MDFe.emit.xFant   := Leitor.rCampo(tcStr, 'xFant');

    if Leitor.rExtrai(2, 'enderEmit') <> '' then
    begin
      MDFe.emit.enderemit.xLgr    := Leitor.rCampo(tcStr, 'xLgr');
      MDFe.emit.enderemit.Nro     := Leitor.rCampo(tcStr, 'nro');
      MDFe.emit.enderemit.xCpl    := Leitor.rCampo(tcStr, 'xCpl');
      MDFe.emit.enderemit.xBairro := Leitor.rCampo(tcStr, 'xBairro');
      MDFe.emit.enderemit.cMun    := Leitor.rCampo(tcInt, 'cMun');
      MDFe.emit.enderemit.xMun    := Leitor.rCampo(tcStr, 'xMun');
      MDFe.emit.enderemit.CEP     := Leitor.rCampo(tcInt, 'CEP');
      MDFe.emit.enderemit.UF      := Leitor.rCampo(tcStr, 'UF');
      MDFe.emit.enderemit.fone    := Leitor.rCampo(tcStr, 'fone');
      MDFe.emit.enderemit.email   := Leitor.rCampo(tcStr, 'email');
    end;
  end;

  (* Grupo da TAG <rodo> ******************************************************)
  if Leitor.rExtrai(1, 'infModal') <> '' then
  begin
    if Leitor.rExtrai(2, 'rodo') <> '' then
    begin
      MDFe.Rodo.RNTRC      := Leitor.rCampo(tcStr, 'RNTRC', 'prop');
      MDFe.Rodo.CIOT       := Leitor.rCampo(tcStr, 'CIOT');
      MDFe.Rodo.codAgPorto := Leitor.rCampo(tcStr, 'codAgPorto');

      if MDFe.infMDFe.versao >= 3 then
      begin
        if (Leitor.rExtrai(3, 'infANTT') <> '') then
        begin
          MDFe.Rodo.infANTT.RNTRC := Leitor.rCampo(tcStr, 'RNTRC');

          i01 := 0;
          MDFe.Rodo.infANTT.infCIOT.Clear;
          while Leitor.rExtrai(4, 'infCIOT', '', i01 + 1) <> '' do
          begin
            MDFe.Rodo.infANTT.infCIOT.New;
            MDFe.Rodo.infANTT.infCIOT[i01].CIOT := Leitor.rCampo(tcStr, 'CIOT');
            MDFe.Rodo.infANTT.infCIOT[i01].CNPJCPF := Leitor.rCampoCNPJCPF;
            inc(i01);
          end;

          if Leitor.rExtrai(4, 'valePed') <> '' then
          begin
            MDFe.Rodo.infANTT.valePed.categCombVeic := StrTocategCombVeic(ok, Leitor.rCampo(tcStr, 'categCombVeic'));

            i01 := 0;
            MDFe.Rodo.infANTT.valePed.disp.Clear;
            while Leitor.rExtrai(5, 'disp', '', i01 + 1) <> '' do
            begin
              MDFe.Rodo.infANTT.valePed.disp.New;
              MDFe.Rodo.infANTT.valePed.disp[i01].CNPJForn := Leitor.rCampo(tcStr, 'CNPJForn');
              MDFe.Rodo.infANTT.valePed.disp[i01].CNPJPg   := Leitor.rCampo(tcStr, 'CNPJPg');

              if MDFe.Rodo.infANTT.valePed.disp[i01].CNPJPg = '' then
                MDFe.Rodo.infANTT.valePed.disp[i01].CNPJPg := Leitor.rCampo(tcStr, 'CPFPg');

              MDFe.Rodo.infANTT.valePed.disp[i01].nCompra   := Leitor.rCampo(tcStr, 'nCompra');
              MDFe.Rodo.infANTT.valePed.disp[i01].vValePed  := Leitor.rCampo(tcDe2, 'vValePed');
              MDFe.Rodo.infANTT.valePed.disp[i01].tpValePed := StrTotpValePed(ok, Leitor.rCampo(tcStr, 'tpValePed'));

              inc(i01);
            end;
          end;

          i01 := 0;
          MDFe.Rodo.infANTT.infContratante.Clear;
          while Leitor.rExtrai(4, 'infContratante', '', i01 + 1) <> '' do
          begin
            MDFe.Rodo.infANTT.infContratante.New;
            MDFe.rodo.infANTT.infContratante[i01].xNome         := Leitor.rCampo(tcStr, 'xNome');
            MDFe.rodo.infANTT.infContratante[i01].idEstrangeiro := Leitor.rCampo(tcStr, 'idEstrangeiro');

            if MDFe.rodo.infANTT.infContratante[i01].idEstrangeiro = '' then
              MDFe.Rodo.infANTT.infContratante[i01].CNPJCPF := Leitor.rCampoCNPJCPF;

            MDFe.rodo.infANTT.infContratante[i01].infContrato.NroContrato := Leitor.rCampo(tcStr, 'NroContrato');
            MDFe.Rodo.infANTT.infContratante[i01].infContrato.vContratoGlobal := Leitor.rCampo(tcDe2, 'vContratoGlobal');

            inc(i01);
          end;

          i01 := 0;
          MDFe.Rodo.infANTT.infPag.Clear;
          while Leitor.rExtrai(4, 'infPag', '', i01 + 1) <> '' do
          begin
            MDFe.Rodo.infANTT.infPag.New;
            MDFe.rodo.infANTT.infPag[i01].xNome         := Leitor.rCampo(tcStr, 'xNome');
            MDFe.rodo.infANTT.infPag[i01].idEstrangeiro := Leitor.rCampo(tcStr, 'idEstrangeiro');

            if MDFe.rodo.infANTT.infPag[i01].idEstrangeiro = '' then
              MDFe.Rodo.infANTT.infPag[i01].CNPJCPF := Leitor.rCampoCNPJCPF;

            MDFe.rodo.infANTT.infPag[i01].vContrato     := Leitor.rCampo(tcDe2, 'vContrato');
            MDFe.rodo.infANTT.infPag[i01].indAltoDesemp := StrToindAltoDesemp(ok, Leitor.rCampo(tcStr, 'indAltoDesemp'));
            MDFe.rodo.infANTT.infPag[i01].indPag        := StrToTIndPag(ok, Leitor.rCampo(tcStr, 'indPag'));
            MDFe.rodo.infANTT.infPag[i01].vAdiant       := Leitor.rCampo(tcDe2, 'vAdiant');

            if Leitor.rCampo(tcStr, 'indAntecipaAdiant') <> '' then
              MDFe.rodo.infANTT.infPag[i01].indAntecipaAdiant := StrToTIndicador(ok, Leitor.rCampo(tcStr, 'indAntecipaAdiant'));

            MDFe.rodo.infANTT.infPag[i01].tpAntecip := StrTotpAntecip(ok, Leitor.rCampo(tcStr, 'tpAntecip'));

            i02 := 0;
            while Leitor.rExtrai(5, 'Comp', '', i02 + 1) <> '' do
            begin
              MDFe.Rodo.infANTT.infPag[i01].Comp.New;
              MDFe.Rodo.infANTT.infPag[i01].Comp[i02].tpComp := StrToTComp(ok, Leitor.rCampo(tcStr, 'tpComp'));
              MDFe.Rodo.infANTT.infPag[i01].Comp[i02].vComp  := Leitor.rCampo(tcDe2, 'vComp');
              MDFe.Rodo.infANTT.infPag[i01].Comp[i02].xComp  := Leitor.rCampo(tcStr, 'xComp');

              inc(i02);
            end;

            if MDFe.rodo.infANTT.infPag[i01].indPag = ipPrazo then
            begin
              i02 := 0;
              while Leitor.rExtrai(5, 'infPrazo', '', i02 + 1) <> '' do
              begin
                MDFe.Rodo.infANTT.infPag[i01].infPrazo.New;
                MDFe.Rodo.infANTT.infPag[i01].infPrazo[i02].nParcela := Leitor.rCampo(tcInt, 'nParcela');
                MDFe.Rodo.infANTT.infPag[i01].infPrazo[i02].dVenc    := Leitor.rCampo(tcDat, 'dVenc');
                MDFe.Rodo.infANTT.infPag[i01].infPrazo[i02].vParcela := Leitor.rCampo(tcDe2, 'vParcela');

                inc(i02);
              end;
            end;

            if Leitor.rExtrai(5, 'infBanc') <> '' then
            begin
              MDFe.rodo.infANTT.infPag[i01].infBanc.PIX := Leitor.rCampo(tcStr, 'PIX');

              if MDFe.rodo.infANTT.infPag[i01].infBanc.PIX = '' then
              begin
                MDFe.rodo.infANTT.infPag[i01].infBanc.CNPJIPEF := Leitor.rCampo(tcStr, 'CNPJIPEF');

                if MDFe.rodo.infANTT.infPag[i01].infBanc.CNPJIPEF = '' then
                begin
                  MDFe.rodo.infANTT.infPag[i01].infBanc.codBanco   := Leitor.rCampo(tcStr, 'codBanco');
                  MDFe.rodo.infANTT.infPag[i01].infBanc.codAgencia := Leitor.rCampo(tcStr, 'codAgencia');
                end;
              end;
            end;

            Inc(i01);
          end;
        end;
      end;

      if (Leitor.rExtrai(3, 'veicTracao') <> '') or (Leitor.rExtrai(3, 'veicPrincipal') <> '')then
      begin
        MDFe.Rodo.veicTracao.cInt    := Leitor.rCampo(tcStr, 'cInt');
        MDFe.Rodo.veicTracao.placa   := Leitor.rCampo(tcStr, 'placa');
        MDFe.Rodo.veicTracao.RENAVAM := Leitor.rCampo(tcStr, 'RENAVAM');
        MDFe.Rodo.veicTracao.tara    := Leitor.rCampo(tcInt, 'tara');
        MDFe.Rodo.veicTracao.capKG   := Leitor.rCampo(tcInt, 'capKG');
        MDFe.Rodo.veicTracao.capM3   := Leitor.rCampo(tcInt, 'capM3');
        MDFe.rodo.veicTracao.tpRod   := StrToTpRodado(ok, Leitor.rCampo(tcStr, 'tpRod'));
        MDFe.rodo.veicTracao.tpCar   := StrToTpCarroceria(ok, Leitor.rCampo(tcStr, 'tpCar'));

        if pos('<prop>', Leitor.Grupo) = 0 then
          MDFe.rodo.veicTracao.UF := Leitor.rCampo(tcStr, 'UF')
        else if (copy(Leitor.Grupo, (Pos('</tpCar>', Leitor.Grupo)+8), 4) = '<UF>') then
          MDFe.rodo.veicTracao.UF := copy(Leitor.Grupo, (Pos('</tpCar>', Leitor.Grupo)+12), 2);

        if Leitor.rExtrai(4, 'prop') <> '' then
        begin
          MDFe.rodo.veicTracao.prop.CNPJCPF := Leitor.rCampoCNPJCPF;
          MDFe.rodo.veicTracao.prop.RNTRC   := Leitor.rCampo(tcStr, 'RNTRC');
          MDFe.rodo.veicTracao.prop.xNome   := Leitor.rCampo(tcStr, 'xNome');
          MDFe.rodo.veicTracao.prop.IE      := Leitor.rCampo(tcStr, 'IE');
          MDFe.rodo.veicTracao.prop.UF      := Leitor.rCampo(tcStr, 'UF');
          MDFe.rodo.veicTracao.prop.tpProp  := StrToTpProp(ok, Leitor.rCampo(tcStr, 'tpProp'));
        end;

        i01 := 0;
        MDFe.rodo.veicTracao.condutor.Clear;
        while Leitor.rExtrai(4, 'condutor', '', i01 + 1) <> '' do
        begin
          MDFe.rodo.veicTracao.condutor.New;
          MDFe.rodo.veicTracao.condutor[i01].xNome := Leitor.rCampo(tcStr, 'xNome');
          MDFe.rodo.veicTracao.condutor[i01].CPF   := Leitor.rCampo(tcStr, 'CPF');
          inc(i01);
        end;
      end;

      i01 := 0;
      MDFe.rodo.veicReboque.Clear;
      while Leitor.rExtrai(3, 'veicReboque', '', i01 + 1) <> '' do
      begin
        MDFe.rodo.veicReboque.New;
        MDFe.rodo.veicReboque[i01].cInt    := Leitor.rCampo(tcStr, 'cInt');
        MDFe.Rodo.veicReboque[i01].placa   := Leitor.rCampo(tcStr, 'placa');
        MDFe.Rodo.veicReboque[i01].RENAVAM := Leitor.rCampo(tcStr, 'RENAVAM');
        MDFe.Rodo.veicReboque[i01].tara    := Leitor.rCampo(tcInt, 'tara');
        MDFe.Rodo.veicReboque[i01].capKG   := Leitor.rCampo(tcInt, 'capKG');
        MDFe.Rodo.veicReboque[i01].capM3   := Leitor.rCampo(tcInt, 'capM3');
        MDFe.rodo.veicReboque[i01].tpCar   := StrToTpCarroceria(ok, Leitor.rCampo(tcStr, 'tpCar'));

        if pos('<prop>', Leitor.Grupo) = 0 then
          MDFe.rodo.veicReboque[i01].UF := Leitor.rCampo(tcStr, 'UF')
        else if (copy(Leitor.Grupo, (Pos('</tpCar>', Leitor.Grupo)+8), 4) = '<UF>') then
          MDFe.rodo.veicReboque[i01].UF := copy(Leitor.Grupo, (Pos('</tpCar>', Leitor.Grupo)+12), 2);

        if Leitor.rExtrai(4, 'prop') <> '' then
        begin
          MDFe.rodo.veicReboque[i01].prop.CNPJCPF := Leitor.rCampoCNPJCPF;
          MDFe.rodo.veicReboque[i01].prop.RNTRC   := Leitor.rCampo(tcStr, 'RNTRC');
          MDFe.rodo.veicReboque[i01].prop.xNome   := Leitor.rCampo(tcStr, 'xNome');
          MDFe.rodo.veicReboque[i01].prop.IE      := Leitor.rCampo(tcStr, 'IE');
          MDFe.rodo.veicReboque[i01].prop.UF      := Leitor.rCampo(tcStr, 'UF');
          MDFe.rodo.veicReboque[i01].prop.tpProp  := StrToTpProp(ok, Leitor.rCampo(tcStr, 'tpProp'));
        end;

        inc(i01);
      end;

      if MDFe.infMDFe.versao < 3 then
      begin
        if Leitor.rExtrai(3, 'valePed') <> '' then
        begin
          i01 := 0;
          MDFe.Rodo.valePed.disp.Clear;
          while Leitor.rExtrai(4, 'disp', '', i01 + 1) <> '' do
          begin
            MDFe.Rodo.valePed.disp.New;
            MDFe.Rodo.valePed.disp[i01].CNPJForn := Leitor.rCampo(tcStr, 'CNPJForn');
            MDFe.Rodo.valePed.disp[i01].CNPJPg   := Leitor.rCampo(tcStr, 'CNPJPg');
            MDFe.Rodo.valePed.disp[i01].nCompra  := Leitor.rCampo(tcStr, 'nCompra');

            inc(i01);
          end;
        end;
      end;

    end; // fim das informações do modal Rodoviário

    (* Grupo da TAG <aereo> ***************************************************)
    if Leitor.rExtrai(2, 'aereo') <> '' then
    begin
      MDFe.Aereo.nac     := Leitor.rCampo(tcStr, 'nac');
      MDFe.Aereo.matr    := Leitor.rCampo(tcStr, 'matr');
      MDFe.Aereo.nVoo    := Leitor.rCampo(tcStr, 'nVoo');
      MDFe.Aereo.cAerEmb := Leitor.rCampo(tcStr, 'cAerEmb');
      MDFe.Aereo.cAerDes := Leitor.rCampo(tcStr, 'cAerDes');
      MDFe.Aereo.dVoo    := Leitor.rCampo(tcDat, 'dVoo');
    end; // fim das informações do modal Aéreo

    (* Grupo da TAG <aquav> ***************************************************)
    if Leitor.rExtrai(2, 'aquav') <> '' then
     begin
       MDFe.aquav.CNPJAgeNav := Leitor.rCampo(tcStr, 'CNPJAgeNav');
       MDFe.aquav.irin       := Leitor.rCampo(tcStr, 'irin');
       MDFe.aquav.tpEmb      := Leitor.rCampo(tcStr, 'tpEmb');
       MDFe.aquav.cEmbar     := Leitor.rCampo(tcStr, 'cEmbar');
       MDFe.aquav.xEmbar     := Leitor.rCampo(tcStr, 'xEmbar');
       MDFe.aquav.nViagem    := Leitor.rCampo(tcStr, 'nViag');
       MDFe.aquav.cPrtEmb    := Leitor.rCampo(tcStr, 'cPrtEmb');
       MDFe.aquav.cPrtDest   := Leitor.rCampo(tcStr, 'cPrtDest');
       MDFe.aquav.prtTrans   := Leitor.rCampo(tcStr, 'prtTrans');
       MDFe.aquav.tpNav      := StrToTpNavegacao(ok, Leitor.rCampo(tcStr, 'tpNav'));

       i01 := 0;
       MDFe.aquav.infTermCarreg.Clear;
       while Leitor.rExtrai(3, 'infTermCarreg', '', i01 + 1) <> '' do
       begin
         MDFe.aquav.infTermCarreg.New;
         MDFe.aquav.infTermCarreg[i01].cTermCarreg := Leitor.rCampo(tcStr, 'cTermCarreg');
         MDFe.aquav.infTermCarreg[i01].xTermCarreg := Leitor.rCampo(tcStr, 'xTermCarreg');
         inc(i01);
       end;

       i01 := 0;
       MDFe.aquav.infTermDescarreg.Clear;
       while Leitor.rExtrai(3, 'infTermDescarreg', '', i01 + 1) <> '' do
       begin
         MDFe.aquav.infTermDescarreg.New;
         MDFe.aquav.infTermDescarreg[i01].cTermDescarreg := Leitor.rCampo(tcStr, 'cTermDescarreg');
         MDFe.aquav.infTermDescarreg[i01].xTermDescarreg := Leitor.rCampo(tcStr, 'xTermDescarreg');
         inc(i01);
       end;

       i01 := 0;
       MDFe.aquav.infEmbComb.Clear;
       while Leitor.rExtrai(3, 'infEmbComb', '', i01 + 1) <> '' do
       begin
         MDFe.aquav.infEmbComb.New;
         MDFe.aquav.infEmbComb[i01].cEmbComb := Leitor.rCampo(tcStr, 'cEmbComb');
         MDFe.aquav.infEmbComb[i01].xBalsa   := Leitor.rCampo(tcStr, 'xBalsa');
         inc(i01);
       end;

       i01 := 0;
       MDFe.aquav.infUnidCargaVazia.Clear;
       while Leitor.rExtrai(3, 'infUnidCargaVazia', '', i01 + 1) <> '' do
       begin
         MDFe.aquav.infUnidCargaVazia.New;
         MDFe.aquav.infUnidCargaVazia[i01].idUnidCargaVazia := Leitor.rCampo(tcStr, 'idUnidCargaVazia');
         MDFe.aquav.infUnidCargaVazia[i01].tpUnidCargaVazia := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCargaVazia'));
         inc(i01);
       end;

       i01 := 0;
       MDFe.aquav.infUnidTranspVazia.Clear;
       while Leitor.rExtrai(3, 'infUnidTranspVazia', '', i01 + 1) <> '' do
       begin
         MDFe.aquav.infUnidTranspVazia.New;
         MDFe.aquav.infUnidTranspVazia[i01].idUnidTranspVazia := Leitor.rCampo(tcStr, 'idUnidTranspVazia');
         MDFe.aquav.infUnidTranspVazia[i01].tpUnidTranspVazia := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTranspVazia'));
         inc(i01);
       end;

     end; // fim das informações do modal Aquaviário

    (* Grupo da TAG <ferrov> **************************************************)
    if Leitor.rExtrai(2, 'ferrov') <> '' then
     begin
       if Leitor.rExtrai(3, 'trem') <> '' then
        begin
         MDFe.ferrov.xPref  := Leitor.rCampo(tcStr, 'xPref');
         MDFe.ferrov.dhTrem := Leitor.rCampo(tcDatHor, 'dhTrem');
         MDFe.ferrov.xOri   := Leitor.rCampo(tcStr, 'xOri');
         MDFe.ferrov.xDest  := Leitor.rCampo(tcStr, 'xDest');
         MDFe.ferrov.qVag   := Leitor.rCampo(tcInt, 'qVag');
        end;

       i01 := 0;
       MDFe.ferrov.vag.Clear;
       while Leitor.rExtrai(3, 'vag', '', i01 + 1) <> '' do
       begin
         MDFe.ferrov.vag.New;
         MDFe.ferrov.vag[i01].pesoBC := Leitor.rCampo(tcDe3, 'pesoBC');
         MDFe.ferrov.vag[i01].pesoR  := Leitor.rCampo(tcDe3, 'pesoR');
         MDFe.ferrov.vag[i01].tpVag  := Leitor.rCampo(tcStr, 'tpVag');
         MDFe.ferrov.vag[i01].serie  := Leitor.rCampo(tcStr, 'serie');
         MDFe.ferrov.vag[i01].nVag   := Leitor.rCampo(tcInt, 'nVag');
         MDFe.ferrov.vag[i01].nSeq   := Leitor.rCampo(tcInt, 'nSeq');
         MDFe.ferrov.vag[i01].TU     := Leitor.rCampo(tcDe3, 'TU');
         inc(i01);
       end;

     end; // fim das informações do modal Ferroviário
   end;

  (* Grupo da TAG <infDoc> ****************************************************)
  if Leitor.rExtrai(1, 'infDoc') <> '' then
  begin
    i01 := 0;
    MDFe.infDoc.infMunDescarga.Clear;
    while Leitor.rExtrai(2, 'infMunDescarga', '', i01 + 1) <> '' do
    begin
      MDFe.infDoc.infMunDescarga.New;
      MDFe.infDoc.infMunDescarga[i01].cMunDescarga := Leitor.rCampo(tcInt, 'cMunDescarga');
      MDFe.infDoc.infMunDescarga[i01].xMunDescarga := Leitor.rCampo(tcStr, 'xMunDescarga');

      i02 := 0;
      while Leitor.rExtrai(3, 'infCTe', '', i02 + 1) <> '' do
      begin
        MDFe.infDoc.infMunDescarga[i01].infCTe.New;
        MDFe.infDoc.infMunDescarga[i01].infCTe[i02].chCTe       := Leitor.rCampo(tcStr, 'chCTe');
        MDFe.infDoc.infMunDescarga[i01].infCTe[i02].SegCodBarra := Leitor.rCampo(tcStr, 'SegCodBarra');

        if MDFe.infMDFe.versao >= 3 then
          MDFe.infDoc.infMunDescarga[i01].infCTe[i02].indReentrega := Leitor.rCampo(tcStr, 'indReentrega');

        i03 := 0;
        while Leitor.rExtrai(4, 'infUnidTransp', '', i03 + 1) <> '' do
        begin
          MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp.New;
          MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
          MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

          // Dentro do grupo <infUnidTransp> podemos ter até duas tags <qtdRat>
          // uma pertencente ao grupo <infUnidCarga> filha de <infUnidTransp> e
          // a outra pertencente ao grupo <infUnidTransp> e ambas são opcionais.
          // precisamos saber se existe uma ocorrência ou duas dessa tag para
          // efetuar a leitura correta das informações.

          sAux := Leitor.Grupo;
          pos1 := PosLast('</infUnidCarga>', sAux);
          pos2 := PosLast('<qtdRat>', sAux) + Length('<qtdRat>');
          pos3 := PosLast('</qtdRat>', sAux);

//          if (pos1 = 0) and (pos2 = 0) and (pos3 = 0) or (pos1 > pos3) then
//            qtdRat_UnidTransp := 0.0;

          if (pos1 < pos3) then
            qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2, pos3 - pos2), 0)
          else
            qtdRat_UnidTransp := 0.0;

          MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].qtdRat := qtdRat_UnidTransp;

          i04 := 0;
          while Leitor.rExtrai(5, 'lacUnidTransp', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].lacUnidTransp.New;
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].lacUnidTransp[i04].nLacre := Leitor.rCampo(tcStr, 'nLacre');
            inc(i04);
          end;

          i04 := 0;
          while Leitor.rExtrai(5, 'infUnidCarga', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].infUnidCarga.New;
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].infUnidCarga[i04].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].infUnidCarga[i04].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].infUnidCarga[i04].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i05 := 0;
            while Leitor.rExtrai(6, 'lacUnidCarga', '', i05 + 1) <> '' do
            begin
              MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga.New;
              MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga[i05].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i05);
            end;

            inc(i04);
          end;

          inc(i03);
        end;

        if MDFe.infMDFe.versao >= 3 then
        begin
          i03 := 0;
          while Leitor.rExtrai(4, 'peri', '', i03 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].peri.New;
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].peri[i03].nONU      := Leitor.rCampo(tcStr, 'nONU');
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].peri[i03].xNomeAE   := Leitor.rCampo(tcStr, 'xNomeAE');
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].peri[i03].xClaRisco := Leitor.rCampo(tcStr, 'xClaRisco');
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].peri[i03].grEmb     := Leitor.rCampo(tcStr, 'grEmb');
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].peri[i03].qTotProd  := Leitor.rCampo(tcStr, 'qTotProd');
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].peri[i03].qVolTipo  := Leitor.rCampo(tcStr, 'qVolTipo');
            inc(i03);
          end;

          if Leitor.rExtrai(4, 'infEntregaParcial') <> '' then
          begin
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infEntregaParcial.qtdTotal   := Leitor.rCampo(tcDe4, 'qtdTotal');
            MDFe.infDoc.infMunDescarga[i01].infCTe[i02].infEntregaParcial.qtdParcial := Leitor.rCampo(tcDe4, 'qtdParcial');
          end;
        end;

        inc(i02);
      end;

      i02 := 0;
      while Leitor.rExtrai(3, 'infCT', '', i02 + 1) <> '' do
      begin
        MDFe.infDoc.infMunDescarga[i01].infCT.New;
        MDFe.infDoc.infMunDescarga[i01].infCT[i02].nCT    := Leitor.rCampo(tcStr, 'nCT');
        MDFe.infDoc.infMunDescarga[i01].infCT[i02].serie  := Leitor.rCampo(tcInt, 'serie');
        MDFe.infDoc.infMunDescarga[i01].infCT[i02].subser := Leitor.rCampo(tcInt, 'subser');
        MDFe.infDoc.infMunDescarga[i01].infCT[i02].dEmi   := Leitor.rCampo(tcDat, 'dEmi');
        MDFe.infDoc.infMunDescarga[i01].infCT[i02].vCarga := Leitor.rCampo(tcDe2, 'vCarga');

        i03 := 0;
        while Leitor.rExtrai(4, 'infUnidTransp', '', i03 + 1) <> '' do
        begin
          MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp.New;
          MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
          MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

          // Dentro do grupo <infUnidTransp> podemos ter até duas tags <qtdRat>
          // uma pertencente ao grupo <infUnidCarga> filha de <infUnidTransp> e
          // a outra pertencente ao grupo <infUnidTransp> e ambas são opcionais.
          // precisamos saber se existe uma ocorrência ou duas dessa tag para
          // efetuar a leitura correta das informações.

          sAux := Leitor.Grupo;
          pos1 := PosLast('</infUnidCarga>', sAux);
          pos2 := PosLast('<qtdRat>', sAux) + Length('<qtdRat>');
          pos3 := PosLast('</qtdRat>', sAux);

//          if (pos1 = 0) and (pos2 = 0) and (pos3 = 0) or (pos1 > pos3) then
//            qtdRat_UnidTransp := 0.0;

          if (pos1 < pos3) then
            qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2, pos3 - pos2), 0)
          else
            qtdRat_UnidTransp := 0.0;

          MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].qtdRat := qtdRat_UnidTransp;

          i04 := 0;
          while Leitor.rExtrai(5, 'lacUnidTransp', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].lacUnidTransp.New;
            MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].lacUnidTransp[i04].nLacre := Leitor.rCampo(tcStr, 'nLacre');
            inc(i04);
          end;

          i04 := 0;
          while Leitor.rExtrai(5, 'infUnidCarga', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].infUnidCarga.New;
            MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].infUnidCarga[i04].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].infUnidCarga[i04].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].infUnidCarga[i04].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i05 := 0;
            while Leitor.rExtrai(6, 'lacUnidCarga', '', i05 + 1) <> '' do
            begin
              MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga.New;
              MDFe.infDoc.infMunDescarga[i01].infCT[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga[i05].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i05);
            end;

            inc(i04);
          end;

          inc(i03);
        end;

        inc(i02);
      end;

      i02 := 0;
      while Leitor.rExtrai(3, 'infNFe', '', i02 + 1) <> '' do
      begin
        MDFe.infDoc.infMunDescarga[i01].infNFe.New;
        MDFe.infDoc.infMunDescarga[i01].infNFe[i02].chNFe       := Leitor.rCampo(tcStr, 'chNFe');
        MDFe.infDoc.infMunDescarga[i01].infNFe[i02].SegCodBarra := Leitor.rCampo(tcStr, 'SegCodBarra');

        if MDFe.infMDFe.versao >= 3 then
          MDFe.infDoc.infMunDescarga[i01].infNFe[i02].indReentrega := Leitor.rCampo(tcStr, 'indReentrega');

        i03 := 0;
        while Leitor.rExtrai(4, 'infUnidTransp', '', i03 + 1) <> '' do
        begin
          MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp.New;
          MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
          MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

          // Dentro do grupo <infUnidTransp> podemos ter até duas tags <qtdRat>
          // uma pertencente ao grupo <infUnidCarga> filha de <infUnidTransp> e
          // a outra pertencente ao grupo <infUnidTransp> e ambas são opcionais.
          // precisamos saber se existe uma ocorrência ou duas dessa tag para
          // efetuar a leitura correta das informações.

          sAux := Leitor.Grupo;
          pos1 := PosLast('</infUnidCarga>', sAux);
          pos2 := PosLast('<qtdRat>', sAux) + Length('<qtdRat>');
          pos3 := PosLast('</qtdRat>', sAux);

//          if (pos1 = 0) and (pos2 = 0) and (pos3 = 0) or (pos1 > pos3) then
//            qtdRat_UnidTransp := 0.0;

          if (pos1 < pos3) then
            qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2, pos3 - pos2), 0)
          else
            qtdRat_UnidTransp := 0.0;

          MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].qtdRat := qtdRat_UnidTransp;

          i04 := 0;
          while Leitor.rExtrai(5, 'lacUnidTransp', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].lacUnidTransp.New;
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].lacUnidTransp[i04].nLacre := Leitor.rCampo(tcStr, 'nLacre');
            inc(i04);
          end;

          i04 := 0;
          while Leitor.rExtrai(5, 'infUnidCarga', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].infUnidCarga.New;
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].infUnidCarga[i04].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].infUnidCarga[i04].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].infUnidCarga[i04].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i05 := 0;
            while Leitor.rExtrai(6, 'lacUnidCarga', '', i05 + 1) <> '' do
            begin
              MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga.New;
              MDFe.infDoc.infMunDescarga[i01].infNFe[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga[i05].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i05);
            end;

            inc(i04);
          end;

          inc(i03);
        end;

        if MDFe.infMDFe.versao >= 3 then
        begin
          i03 := 0;
          while Leitor.rExtrai(4, 'peri', '', i03 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].peri.New;
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].peri[i03].nONU      := Leitor.rCampo(tcStr, 'nONU');
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].peri[i03].xNomeAE   := Leitor.rCampo(tcStr, 'xNomeAE');
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].peri[i03].xClaRisco := Leitor.rCampo(tcStr, 'xClaRisco');
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].peri[i03].grEmb     := Leitor.rCampo(tcStr, 'grEmb');
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].peri[i03].qTotProd  := Leitor.rCampo(tcStr, 'qTotProd');
            MDFe.infDoc.infMunDescarga[i01].infNFe[i02].peri[i03].qVolTipo  := Leitor.rCampo(tcStr, 'qVolTipo');
            inc(i03);
          end;
        end;

        inc(i02);
      end;

      i02 := 0;
      while Leitor.rExtrai(3, 'infNF', '', i02 + 1) <> '' do
      begin
        MDFe.infDoc.infMunDescarga[i01].infNF.New;
        MDFe.infDoc.infMunDescarga[i01].infNF[i02].CNPJ  := Leitor.rCampo(tcStr, 'CNPJ');
        MDFe.infDoc.infMunDescarga[i01].infNF[i02].UF    := Leitor.rCampo(tcStr, 'UF');
        MDFe.infDoc.infMunDescarga[i01].infNF[i02].nNF   := Leitor.rCampo(tcStr, 'nNF');
        MDFe.infDoc.infMunDescarga[i01].infNF[i02].serie := Leitor.rCampo(tcInt, 'serie');
        MDFe.infDoc.infMunDescarga[i01].infNF[i02].dEmi  := Leitor.rCampo(tcDat, 'dEmi');
        MDFe.infDoc.infMunDescarga[i01].infNF[i02].vNF   := Leitor.rCampo(tcDe2, 'vNF');
        MDFe.infDoc.infMunDescarga[i01].infNF[i02].PIN   := Leitor.rCampo(tcInt, 'PIN');

        i03 := 0;
        while Leitor.rExtrai(4, 'infUnidTransp', '', i03 + 1) <> '' do
        begin
          MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp.New;
          MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
          MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

          // Dentro do grupo <infUnidTransp> podemos ter até duas tags <qtdRat>
          // uma pertencente ao grupo <infUnidCarga> filha de <infUnidTransp> e
          // a outra pertencente ao grupo <infUnidTransp> e ambas são opcionais.
          // precisamos saber se existe uma ocorrência ou duas dessa tag para
          // efetuar a leitura correta das informações.

          sAux := Leitor.Grupo;
          pos1 := PosLast('</infUnidCarga>', sAux);
          pos2 := PosLast('<qtdRat>', sAux) + Length('<qtdRat>');
          pos3 := PosLast('</qtdRat>', sAux);

//          if (pos1 = 0) and (pos2 = 0) and (pos3 = 0) or (pos1 > pos3) then
//            qtdRat_UnidTransp := 0.0;

          if (pos1 < pos3) then
            qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2, pos3 - pos2), 0)
          else
            qtdRat_UnidTransp := 0.0;

          MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].qtdRat := qtdRat_UnidTransp;

          i04 := 0;
          while Leitor.rExtrai(5, 'lacUnidTransp', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].lacUnidTransp.New;
            MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].lacUnidTransp[i04].nLacre := Leitor.rCampo(tcStr, 'nLacre');
            inc(i04);
          end;

          i04 := 0;
          while Leitor.rExtrai(5, 'infUnidCarga', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].infUnidCarga.New;
            MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].infUnidCarga[i04].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].infUnidCarga[i04].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].infUnidCarga[i04].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i05 := 0;
            while Leitor.rExtrai(6, 'lacUnidCarga', '', i05 + 1) <> '' do
            begin
              MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga.New;
              MDFe.infDoc.infMunDescarga[i01].infNF[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga[i05].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i05);
            end;

            inc(i04);
          end;

          inc(i03);
        end;

        inc(i02);
      end;

      i02 := 0;
      while Leitor.rExtrai(3, 'infMDFeTransp', '', i02 + 1) <> '' do
      begin
        MDFe.infDoc.infMunDescarga[i01].infMDFeTransp.New;
        MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].chMDFe := Leitor.rCampo(tcStr, 'chMDFe');

        if MDFe.infMDFe.versao >= 3 then
          MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].indReentrega := Leitor.rCampo(tcStr, 'indReentrega');

        i03 := 0;
        while Leitor.rExtrai(4, 'infUnidTransp', '', i03 + 1) <> '' do
        begin
          MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp.New;
          MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].tpUnidTransp := StrToUnidTransp(ok, Leitor.rCampo(tcStr, 'tpUnidTransp'));
          MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].idUnidTransp := Leitor.rCampo(tcStr, 'idUnidTransp');

          // Dentro do grupo <infUnidTransp> podemos ter até duas tags <qtdRat>
          // uma pertencente ao grupo <infUnidCarga> filha de <infUnidTransp> e
          // a outra pertencente ao grupo <infUnidTransp> e ambas são opcionais.
          // precisamos saber se existe uma ocorrência ou duas dessa tag para
          // efetuar a leitura correta das informações.

          sAux := Leitor.Grupo;
          pos1 := PosLast('</infUnidCarga>', sAux);
          pos2 := PosLast('<qtdRat>', sAux) + Length('<qtdRat>');
          pos3 := PosLast('</qtdRat>', sAux);

//          if (pos1 = 0) and (pos2 = 0) and (pos3 = 0) or (pos1 > pos3) then
//            qtdRat_UnidTransp := 0.0;

          if (pos1 < pos3) then
            qtdRat_UnidTransp := StringToFloatDef(Copy(sAux, pos2, pos3 - pos2), 0)
          else
            qtdRat_UnidTransp := 0.0;

          MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].qtdRat := qtdRat_UnidTransp;

          i04 := 0;
          while Leitor.rExtrai(5, 'lacUnidTransp', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].lacUnidTransp.New;
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].lacUnidTransp[i04].nLacre := Leitor.rCampo(tcStr, 'nLacre');
            inc(i04);
          end;

          i04 := 0;
          while Leitor.rExtrai(5, 'infUnidCarga', '', i04 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].infUnidCarga.New;
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].infUnidCarga[i04].tpUnidCarga := StrToUnidCarga(ok, Leitor.rCampo(tcStr, 'tpUnidCarga'));
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].infUnidCarga[i04].idUnidCarga := Leitor.rCampo(tcStr, 'idUnidCarga');
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].infUnidCarga[i04].qtdRat      := Leitor.rCampo(tcDe2, 'qtdRat');

            i05 := 0;
            while Leitor.rExtrai(6, 'lacUnidCarga', '', i05 + 1) <> '' do
            begin
              MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga.New;
              MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].infUnidTransp[i03].infUnidCarga[i04].lacUnidCarga[i05].nLacre := Leitor.rCampo(tcStr, 'nLacre');
              inc(i05);
            end;

            inc(i04);
          end;

          inc(i03);
        end;

        if MDFe.infMDFe.versao >= 3 then
        begin
          i03 := 0;
          while Leitor.rExtrai(4, 'peri', '', i03 + 1) <> '' do
          begin
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].peri.New;
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].peri[i03].nONU      := Leitor.rCampo(tcStr, 'nONU');
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].peri[i03].xNomeAE   := Leitor.rCampo(tcStr, 'xNomeAE');
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].peri[i03].xClaRisco := Leitor.rCampo(tcStr, 'xClaRisco');
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].peri[i03].grEmb     := Leitor.rCampo(tcStr, 'grEmb');
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].peri[i03].qTotProd  := Leitor.rCampo(tcStr, 'qTotProd');
            MDFe.infDoc.infMunDescarga[i01].infMDFeTransp[i02].peri[i03].qVolTipo  := Leitor.rCampo(tcStr, 'qVolTipo');
            inc(i03);
          end;
        end;

        inc(i02);
      end;

      inc(i01);
    end;
  end;

  if MDFe.infMDFe.versao >= 3 then
  begin
    i01 := 0;
    MDFe.seg.Clear;
    while Leitor.rExtrai(1, 'seg', '', i01 + 1) <> '' do
    begin
      MDFe.seg.New;

      MDFe.seg[i01].nApol   := Leitor.rCampo(tcStr, 'nApol');

      i02 := 0;
      while Leitor.rExtrai(2, 'nAver', '', i02 + 1) <> '' do
      begin
        MDFe.seg[i01].aver.New;
        MDFe.seg[i01].aver[i02].nAver := Leitor.rCampo(tcStr, 'nAver');
        inc(i02);
      end;

      if (Leitor.rExtrai(2, 'infResp') <> '') then
      begin
        MDFe.seg[i01].respSeg := StrToRspSeguroMDFe(ok, Leitor.rCampo(tcStr, 'respSeg'));
        MDFe.seg[i01].CNPJCPF := Leitor.rCampoCNPJCPF;
      end;
      if (Leitor.rExtrai(2, 'infSeg') <> '') then
      begin
        MDFe.seg[i01].xSeg    := Leitor.rCampo(tcStr, 'xSeg');
        MDFe.seg[i01].CNPJ    := Leitor.rCampo(tcStr, 'CNPJ');
      end;

      inc(i01);
    end;

    if Leitor.rExtrai(1, 'prodPred') <> '' then
    begin
      MDFe.prodPred.tpCarga := StrToTCarga(ok, Leitor.rCampo(tcStr, 'tpCarga'));
      MDFe.prodPred.xProd   := Leitor.rCampo(tcStr, 'xProd');
      MDFe.prodPred.cEAN    := Leitor.rCampo(tcStr, 'cEAN');
      MDFe.prodPred.NCM     := Leitor.rCampo(tcStr, 'NCM');

      if Leitor.rExtrai(2, 'infLotacao') <> '' then
      begin
        if Leitor.rExtrai(3, 'infLocalCarrega') <> '' then
        begin
          MDFe.prodPred.infLocalCarrega.CEP       := Leitor.rCampo(tcInt, 'CEP');
          MDFe.prodPred.infLocalCarrega.latitude  := Leitor.rCampo(tcDe6, 'latitude');
          MDFe.prodPred.infLocalCarrega.longitude := Leitor.rCampo(tcDe6, 'longitude');
        end;

        if Leitor.rExtrai(3, 'infLocalDescarrega') <> '' then
        begin
          MDFe.prodPred.infLocalDescarrega.CEP       := Leitor.rCampo(tcInt, 'CEP');
          MDFe.prodPred.infLocalDescarrega.latitude  := Leitor.rCampo(tcDe6, 'latitude');
          MDFe.prodPred.infLocalDescarrega.longitude := Leitor.rCampo(tcDe6, 'longitude');
        end;
      end;
    end;
  end;

  (* Grupo da TAG <tot> *******************************************************)
  if Leitor.rExtrai(1, 'tot') <> '' then
  begin
   MDFe.tot.qCTe   := Leitor.rCampo(tcInt, 'qCTe');
   MDFe.tot.qCT    := Leitor.rCampo(tcInt, 'qCT');
   MDFe.tot.qNFe   := Leitor.rCampo(tcInt, 'qNFe');
   MDFe.tot.qNF    := Leitor.rCampo(tcInt, 'qNF');
   MDFe.tot.qMDFe  := Leitor.rCampo(tcInt, 'qMDFe');
   MDFe.tot.vCarga := Leitor.rCampo(tcDe2, 'vCarga');
   MDFe.tot.cUnid  := StrToUnidMed(Ok, Leitor.rCampo(tcStr, 'cUnid'));
   MDFe.tot.qCarga := Leitor.rCampo(tcDe4, 'qCarga');
  end;

  (* Grupo da TAG <lacres> ****************************************************)
  i01 := 0;
  MDFe.lacres.Clear;
  while Leitor.rExtrai(1, 'lacres', '', i01 + 1) <> '' do
  begin
    MDFe.lacres.New;
    MDFe.lacres[i01].nLacre := Leitor.rCampo(tcStr, 'nLacre');
    inc(i01);
  end;

  (* Grupo da TAG <autXML> ****************************************************)
  i01 := 0;
  MDFe.autXML.Clear;
  while Leitor.rExtrai(1, 'autXML', '', i01 + 1) <> '' do
  begin
    MDFe.autXML.New;
    MDFe.autXML[i01].CNPJCPF := Leitor.rCampoCNPJCPF;;
    inc(i01);
  end;

  (* Grupo da TAG <infAdic> ***************************************************)
  if Leitor.rExtrai(1, 'infAdic') <> '' then
  begin
    MDFe.infAdic.infAdFisco := Leitor.rCampo(tcStr, 'infAdFisco');
    MDFe.infAdic.infCpl     := Leitor.rCampo(tcStr, 'infCpl');
  end;

  if Leitor.rExtrai(1, 'infRespTec') <> '' then
  begin
    MDFe.infRespTec.CNPJ     := Leitor.rCampo(tcStr, 'CNPJ');
    MDFe.infRespTec.xContato := Leitor.rCampo(tcStr, 'xContato');
    MDFe.infRespTec.email    := Leitor.rCampo(tcStr, 'email');
    MDFe.infRespTec.fone     := Leitor.rCampo(tcStr, 'fone');
    MDFe.infRespTec.idCSRT   := Leitor.rCampo(tcInt, 'idCSRT');
    MDFe.infRespTec.hashCSRT := Leitor.rCampo(tcStr, 'hashCSRT');
  end;

  (* Grupo da TAG <signature> *************************************************)

  Leitor.Grupo := Leitor.Arquivo;

  MDFe.signature.URI             := Leitor.rAtributo('URI=', 'Reference');
  MDFe.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
  MDFe.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
  MDFe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

  (* Grupo da TAG <infMDFeSupl> ************************************************)
  if Leitor.rExtrai(1, 'infMDFeSupl') <> '' then
  begin
    MDFe.infMDFeSupl.qrCodMDFe := Leitor.rCampo(tcStr, 'qrCodMDFe');
    MDFe.infMDFeSupl.qrCodMDFe := StringReplace(MDFe.infMDFeSupl.qrCodMDFe, '<![CDATA[', '', []);
    MDFe.infMDFeSupl.qrCodMDFe := StringReplace(MDFe.infMDFeSupl.qrCodMDFe, ']]>', '', []);
  end;

  (* Grupo da TAG <protMDFe> **************************************************)
  if Leitor.rExtrai(1, 'protMDFe') <> '' then
  begin
    MDFe.procMDFe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
    MDFe.procMDFe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
    MDFe.procMDFe.chMDFe   := Leitor.rCampo(tcStr, 'chMDFe');
    MDFe.procMDFe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
    MDFe.procMDFe.nProt    := Leitor.rCampo(tcStr, 'nProt');
    MDFe.procMDFe.digVal   := Leitor.rCampo(tcStr, 'digVal');
    MDFe.procMDFe.cStat    := Leitor.rCampo(tcInt, 'cStat');
    MDFe.procMDFe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
    MDFe.procMDFe.cMsg     := Leitor.rCampo(tcInt, 'cMsg');
    MDFe.procMDFe.xMsg     := Leitor.rCampo(tcStr, 'xMsg');
  end;

  Result := True;
end;

end.

