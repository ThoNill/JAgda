package tho.nill.JAgda;

import static org.junit.Assert.*;

import org.junit.Test;

public class AgdaTypeTest {

	@Test
	public void indexTest() {
		AgdaModul modul = AgdaModul.NN;
		WithConstructorsType natType = new WithConstructorsType("Nat",true,modul,AgdaType.SET);
		SimpleConstructor zero = new SimpleConstructor("zero",true,natType,natType);
		FunctionalConstructor suc = new FunctionalConstructor("suc",true,natType,
				new FunctionType(true,modul,natType, natType,AgdaType.SET));
		AgdaInstance n0 = zero.apply(null);
		AgdaInstance n1 = suc.apply(n0);
		AgdaInstance n2 = suc.apply(n1);
		AgdaInstance n3 = suc.apply(n2);
		AgdaInstance n4 = suc.apply(n3);
		assertTrue(n2.leq(n3));
		assertTrue(n0.leq(n3));
		assertTrue(n3.leq(n3));
		assertTrue(n4.leq(n4));
		assertFalse(n3.leq(n1));
	}
	
	@Test
	public void indexedTypeTest() {
		AgdaModul modul = AgdaModul.NN;
		WithConstructorsType natType = new WithConstructorsType("Nat",true,modul,AgdaType.SET);
		SimpleConstructor zero = new SimpleConstructor("zero",true,natType,natType);
		FunctionalConstructor suc = new FunctionalConstructor("suc",true,natType,
				new FunctionType(true,modul,natType, natType,AgdaType.SET));
		AgdaInstance n0 = zero.apply(null);
		AgdaInstance n1 = suc.apply(n0);
		AgdaInstance n2 = suc.apply(n1);
		AgdaInstance n3 = suc.apply(n2);
		AgdaInstance n4 = suc.apply(n3);
		
		IndexedType indexedType2 = new IndexedType("Vector",true, modul,n2,AgdaType.SET,AgdaType.SET);
		IndexedType indexedType3 = new IndexedType("Vector",true, modul,n3,AgdaType.SET,AgdaType.SET);
		IndexedType indexedType4 = new IndexedType("Vector",true, modul,n4,AgdaType.SET,AgdaType.SET);
		assertTrue(indexedType3.isSubtype(indexedType2));
		assertTrue(indexedType4.isSubtype(indexedType2));
	}

	@Test
	public void labledTypeTest() {
		AgdaType set0 = AgdaType.SET;
		AgdaType set1 = set0.getType();
		AgdaType set2 = set1.getType();
		AgdaType set3 = set2.getType();
		assertTrue(set3.isSubtype(set1));
		assertTrue(set3.isSubtype(set3));
		assertFalse(set3.equals(set1));
		

	}
	
	@Test
	public void equalityUngleichTest() {
		AgdaModul modul = AgdaModul.NN;
		WithConstructorsType natType = new WithConstructorsType("Nat",true,modul,AgdaType.SET);
		SimpleConstructor zero = new SimpleConstructor("zero",true,natType,natType);
		FunctionalConstructor suc = new FunctionalConstructor("suc",true,natType,
				new FunctionType(true,modul,natType, natType,AgdaType.SET));
		AgdaInstance n0 = zero.apply(null);
		AgdaInstance n1 = suc.apply(n0);
		AgdaInstance n2 = suc.apply(n1);
		AgdaInstance n3 = suc.apply(n2);
		AgdaInstance n4 = suc.apply(n3);
		
		DependType x = new DependType("=x",true, modul,n1,AgdaType.SET,AgdaType.SET);
		DependType xy = new DependType("y",true, modul,n4,x,AgdaType.SET);
		
		DependType reflType = new DependType("y",true, modul,n1,x,AgdaType.SET);
		
		
		try {
			SimpleConstructor refl = new SimpleConstructor("refl",true,xy,reflType);
			fail(" hat keine Exception erzeugt ");
		} catch (AgdaException ex) {
			
		}
		
	}
	
	@Test
	public void equalityGleichTest() {
		AgdaModul modul = AgdaModul.NN;
		WithConstructorsType natType = new WithConstructorsType("Nat",true,modul,AgdaType.SET);
		SimpleConstructor zero = new SimpleConstructor("zero",true,natType,natType);
		FunctionalConstructor suc = new FunctionalConstructor("suc",true,natType,
				new FunctionType(true,modul,natType, natType,AgdaType.SET));
		AgdaInstance n0 = zero.apply(null);
		AgdaInstance n1 = suc.apply(n0);
		AgdaInstance n2 = suc.apply(n1);
		AgdaInstance n3 = suc.apply(n2);
		AgdaInstance n4 = suc.apply(n3);
		
		DependType x = new DependType("=x",true, modul,n4,AgdaType.SET,AgdaType.SET);
		DependType xy = new DependType("y",true, modul,n4,x,AgdaType.SET);
		
		DependType reflType = new DependType("y",true, modul,n4,x,AgdaType.SET);
		
		
		try {
			SimpleConstructor refl = new SimpleConstructor("refl",true,xy,reflType);
		} catch (AgdaException ex) {
			fail(" hat eine Exception erzeugt ");
		}
		
	}

}
