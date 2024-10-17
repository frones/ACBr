package com.acbr.nfe.acbrlibnfe.demo.configuracoes;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.Spinner;

import androidx.fragment.app.Fragment;

import com.acbr.nfe.acbrlibnfe.demo.utils.ACBrLibHelper;
import com.acbr.nfe.acbrlibnfe.demo.R;
import com.acbr.nfe.acbrlibnfe.demo.utils.SpinnerUtils;

import br.com.acbr.lib.nfe.ACBrLibNFe;
import br.com.acbr.lib.nfe.TipoDANFE;
import br.com.acbr.lib.nfe.TipoRelatorioBobina;
import br.com.acbr.lib.nfe.TipoRelatorioEvento;

public class ConfiguracoesDocumentoAuxiliarFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;

    private Spinner cmbTipoDanfe;
    private Spinner cmbTipoRelatorioBobina;
    private Spinner cmbTipoRelatorioEvento;
    private Spinner cmbModelo;
    private Spinner cmbPaginaDeCodigo;
    private EditText txtLogomarca;
    private EditText txtPortas;
    private EditText txtColunas;
    private EditText txtEspacos;
    private EditText txtBuffer;
    private EditText txtLinhasPular;
    private CheckBox chkControlePorta;
    private CheckBox chkCortarPapel;
    private CheckBox chkTraduzirTags;
    private CheckBox chkIgnorarTags;
    private Button btnSalvarConfiguracoesDocumentoAuxiliar;
    private Button btnCarregarConfiguracoesDocumentoAuxiliar;

    private TipoDANFE[] tipoDANFE = TipoDANFE.values();
    private TipoRelatorioBobina[] tipoRelatorioBobina = TipoRelatorioBobina.values();
    private TipoRelatorioEvento[] tipoRelatorioEvento = TipoRelatorioEvento.values();
    private String[] modelo = {"ppTexto", "ppEscPosEpson", "ppEscBematech", "ppEscDaruma", "ppEscVox", "ppEscDiebold", "ppEscEpsonP2", "ppCustomPos", "ppEscPosStar", "ppEscZJiang", "ppEscGPrinter", "ppEscDatecs", "ppEscSunmi", "ppExterno"};
    private String[] paginaDeCodigo = {"pcNone", "pc437", "pc850", "pc852", "pc860", "pcUTF8", "pc1252"};

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_documento_auxiliar, container, false);

        ACBrNFe = ACBrLibHelper.getInstance("");

        txtLogomarca = view.findViewById(R.id.txtLogomarca);
        txtPortas = view.findViewById(R.id.txtPortas);
        txtColunas = view.findViewById(R.id.txtColunas);
        txtEspacos = view.findViewById(R.id.txtEspacos);
        txtBuffer = view.findViewById(R.id.txtBuffer);
        txtLinhasPular = view.findViewById(R.id.txtLinhasPular);
        chkControlePorta = view.findViewById(R.id.chkControlePorta);
        chkCortarPapel = view.findViewById(R.id.chkCortarPapel);
        chkTraduzirTags = view.findViewById(R.id.chkTraduzirTags);
        chkIgnorarTags = view.findViewById(R.id.chkIgnorarTags);
        cmbTipoDanfe = view.findViewById(R.id.cmbTipoDanfe);
        cmbTipoRelatorioBobina = view.findViewById(R.id.cmbTipoRelatorioBobina);
        cmbTipoRelatorioEvento = view.findViewById(R.id.cmbTipoRelatorioEvento);
        cmbModelo = view.findViewById(R.id.cmbModelo);
        cmbPaginaDeCodigo = view.findViewById(R.id.cmbPaginaDeCodigo);
        btnSalvarConfiguracoesDocumentoAuxiliar = view.findViewById(R.id.btnSalvarConfiguracoesDocumentoAuxiliar);
        btnCarregarConfiguracoesDocumentoAuxiliar = view.findViewById(R.id.btnCarregarConfiguracoesDocumentoAuxiliar);

        SpinnerUtils.preencherSpinner(getActivity(), cmbTipoDanfe, tipoDANFE);
        SpinnerUtils.preencherSpinner(getActivity(), cmbTipoRelatorioBobina, tipoRelatorioBobina);
        SpinnerUtils.preencherSpinner(getActivity(), cmbTipoRelatorioEvento, tipoRelatorioEvento);
        SpinnerUtils.preencherSpinner(getActivity(), cmbModelo, modelo);
        SpinnerUtils.preencherSpinner(getActivity(), cmbPaginaDeCodigo, paginaDeCodigo);

        btnSalvarConfiguracoesDocumentoAuxiliar.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesDocumentoAuxiliar();
            }
        });

        btnCarregarConfiguracoesDocumentoAuxiliar.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesDocumentoAuxiliar();
            }
        });

        carregarConfiguracoesDocumentoAuxiliar();

        return view;
    }

    private void salvarConfiguracoesDocumentoAuxiliar() {
        try {
            ACBrNFe.configGravarValor("DANFE", "PathLogo", txtLogomarca.getText().toString());
            ACBrNFe.configGravarValor("DANFE", "TipoDANFE", Integer.toString(cmbTipoDanfe.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("DANFENFCe", "TipoRelatorioBobina", Integer.toString(cmbTipoRelatorioBobina.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("DANFENFCe", "TipoRelatorioEvento", Integer.toString(cmbTipoRelatorioEvento.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("PosPrinter", "Modelo", Integer.toString(cmbModelo.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("PosPrinter", "PaginaDeCodigo", Integer.toString(cmbPaginaDeCodigo.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("PosPrinter", "Porta", txtPortas.getText().toString());
            ACBrNFe.configGravarValor("PosPrinter", "ColunasFonteNormal", txtColunas.getText().toString());
            ACBrNFe.configGravarValor("PosPrinter", "EspacoEntreLinhas", txtEspacos.getText().toString());
            ACBrNFe.configGravarValor("PosPrinter", "LinhasBuffer", txtBuffer.getText().toString());
            ACBrNFe.configGravarValor("PosPrinter", "LinhasEntreCupons", txtLinhasPular.getText().toString());
            ACBrNFe.configGravarValor("PosPrinter", "ControlePorta", chkControlePorta.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("PosPrinter", "CortaPapel", chkCortarPapel.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("PosPrinter", "TraduzirTags", chkTraduzirTags.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("PosPrinter", "IgnorarTags", chkIgnorarTags.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravar();
        } catch (Exception ex) {
            Log.i("Erro", " - Salvar Configurações Documento Auxiliar: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesDocumentoAuxiliar() {
        try {
            txtLogomarca.setText(ACBrNFe.configLerValor("DANFe", "PathLogo"));
            cmbTipoDanfe.setSelection(Integer.valueOf(ACBrNFe.configLerValor("DANFe", "TipoDANFE")));
            cmbTipoRelatorioBobina.setSelection(Integer.valueOf(ACBrNFe.configLerValor("DANFENFCe", "TipoRelatorioBobina")));
            cmbTipoRelatorioEvento.setSelection(Integer.valueOf(ACBrNFe.configLerValor("DANFENFCe", "TipoRelatorioEvento")));
            cmbModelo.setSelection(Integer.valueOf(ACBrNFe.configLerValor("PosPrinter", "Modelo")));
            cmbPaginaDeCodigo.setSelection(Integer.valueOf(ACBrNFe.configLerValor("PosPrinter", "PaginaDeCodigo")));
            txtPortas.setText(ACBrNFe.configLerValor("PosPrinter", "Porta"));
            txtColunas.setText(ACBrNFe.configLerValor("PosPrinter", "ColunasFonteNormal"));
            txtEspacos.setText(ACBrNFe.configLerValor("PosPrinter", "EspacoEntreLinhas"));
            txtBuffer.setText(ACBrNFe.configLerValor("PosPrinter", "LinhasBuffer"));
            txtLinhasPular.setText(ACBrNFe.configLerValor("PosPrinter", "LinhasEntreCupons"));
            chkControlePorta.setChecked(Integer.parseInt(ACBrNFe.configLerValor("PosPrinter", "ControlePorta")) == 1);
            chkCortarPapel.setChecked(Integer.parseInt(ACBrNFe.configLerValor("PosPrinter", "CortaPapel")) == 1);
            chkTraduzirTags.setChecked(Integer.parseInt(ACBrNFe.configLerValor("PosPrinter", "TraduzirTags")) == 1);
            chkIgnorarTags.setChecked(Integer.parseInt(ACBrNFe.configLerValor("PosPrinter", "IgnorarTags")) == 1);
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Documento Auxiliar: " + ex.getMessage());
        }
    }
}
