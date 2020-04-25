package tho.nill.JAgda;

import java.util.ArrayList;
import java.util.List;

public class IndexedType extends DependType {
	

	public IndexedType(String name,boolean visible, AgdaModul modul, AgdaInstance parameter, AgdaType target, AgdaType type) {
		super(name,visible, modul, parameter, target, type);
	}

	@Override
	public boolean isSubtype(AgdaType type) {
		if (type instanceof DependType) {
			DependType other = (DependType)type;
			AgdaInstance paramOther = other.getParameter();
			AgdaInstance paramThis = getParameter();
			if (paramOther.getType().equals(paramThis.getType())) {
				return paramOther.leq(paramThis);
			}
		}
		return super.isSubtype(type);
	}
	
	
}
